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
  X[, str_detect(colnames(X), paste0(info$vars$o$names, collapse = '|'))] <- 0
  ds_pred <- data.frame(y = X %*% B,
                        x = info$data$unscaled[, info$vars$x$name],
                        y_ll = X %*% B_ll,
                        y_ul = X %*% B_ul)
  if (is.null(model_fit[['m']])) {
    return(ds_pred)
  }
  for (m in 1:length(names(info$vars$m))) {
    if (info$vars$m[[m]]$type == 'factor') {
      ds_pred[, glue("m{m}")] <- model_fit[['m']][[m]]
    } else if (info$vars$m[[m]]$type == 'numeric') {
      ds_pred[, glue("m{m}")] <- round(model_fit[['m']][[m]], 3)
    }
  }
  return(ds_pred)
}


#' get_ss_pred_all
#'
#' @param model
#' @param x_var
#' @param m_vars
#'
#' @return
#' @export
#' @import dplyr
#' @examples
get_ss_pred_all <- function(model, x_var, m_vars = NULL) {
  info <- get_model_info(model, x_var = x_var, m_vars = m_vars)
  models <- get_models(info, type = 'ss')
  df_coef <- get_interaction_breakdown(model, x_var, m_vars, type = 'ss')
  ds_pred <- lapply(models, function(m) get_ss_pred(m, info = info)) %>%
    abind(along = 1) %>%
    as.data.frame() %>%
    select(contains('m'), everything())
  row.names(ds_pred) <- NULL
  for (j in 1:ncol(ds_pred)) {
      ds_pred[, j] <- as_numeric(ds_pred[, j])
  }
  ds_pred <- as.data.frame(ds_pred)
  return(ds_pred)
}
