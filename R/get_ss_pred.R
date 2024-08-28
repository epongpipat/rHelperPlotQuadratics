#' get_ss_pred
#'
#' @param model_fit
#' @param info
#'
#' @return
#' @export
#' @import stringr
#' @importFrom glue glue
#' @examples
get_ss_pred <- function(model_fit, info) {

  B <- as.matrix(coef(model_fit))
  ds_ci <- confint(model_fit)
  B_ll <- as.matrix(ds_ci[, 1])
  B_ul <- as.matrix(ds_ci[, 2])
  X <- model.matrix(model_fit)
  if (length(info$vars$o$names) > 0) {
    X[, str_detect(colnames(X), paste0(info$vars$o$names, collapse = '|'))] <- 0
  }
  ds_pred <- data.frame(y = X %*% B,
                        x = info$data$unscaled[, info$vars$x$name],
                        y_ll = X %*% B_ll,
                        y_ul = X %*% B_ul)
  if (is.null(info$vars$m)) {
    return(ds_pred)
  }
  for (m in 1:length(names(info$vars$m))) {
    if (info$vars$m[[m]]$type == 'factor') {
      ds_pred[, glue("m{m}")] <- attributes(model_fit)[['m']][[m]]
    } else if (info$vars$m[[m]]$type == 'numeric') {
      ds_pred[, glue("m{m}")] <- round(attributes(model_fit)[['m']][[m]], info$opts$round)
    }
  }
  return(ds_pred)
}


#' get_ss_pred_all
#'
#' @param info
#'
#' @return
#' @export
#' @import dplyr
#' @examples
get_ss_pred_all <- function(info) {
  models <- get_models(info, type = 'ss')
  df_coef <- get_interaction_breakdown(info, type = 'ss')
  data_pred <- lapply(models, function(m) get_ss_pred(m, info = info))
  ds_pred <- data_pred %>%
    abind(along = 1) %>%
    as.data.frame() %>%
    select(contains('m'), 'x', everything()) %>%
    as.data.frame()
  row.names(ds_pred) <- NULL
  for (j in 1:ncol(ds_pred)) {
      ds_pred[, j] <- as_numeric(ds_pred[, j])
  }
  ds_pred <- as.data.frame(ds_pred)
  return(ds_pred)
}
