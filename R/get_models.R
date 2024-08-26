#' get_models
#'
#' @param info
#' @param type
#'
#' @return
#' @export
#' @examples
get_models <- function(info, type = NULL) {
  models <- list()
  if (is.null(info$vars$m) & type == 'ss') {
    models[[1]] <- lm(as.formula(info$vars$formula[[type]]), get_data(info, 1, type = type))
    return(models)
  }
  for (i in 1:nrow(info$grid[[type]])) {
    models[[i]] <- lm(as.formula(info$vars$formula[[type]]), get_data(info, i, type = type))
    for (j in 1:ncol(info$grid[[type]])) {
      temp <- list()
      temp$var <- colnames(info$grid[[type]])[j]
      temp$value <- info$grid[[type]][i, j]
      if (temp$var == info$vars$x$name) {
        attr(models[[i]], 'x') <- temp$value
      } else {
        attr(models[[i]], 'm')[[temp$var]] <- temp$value
      }
    }
  }
  return(models)
}
