#' get_data
#'
#' @param info
#' @param i
#' @param type
#'
#' @return
#' @export
#' @importFrom tidyr contains
#' @import dplyr
#' @examples
get_data <- function(info, i, type = NULL) {

  # ----------------------------------------------------------------------------
  # checks
  # ----------------------------------------------------------------------------
  if (is.null(type)) {
    stop("type must be specified as either 'jn' or 'ss'")
  }
  if (!(type %in% c('jn', 'ss'))) {
    stop("type must be either 'jn' or 'ss'")
  }

  # ----------------------------------------------------------------------------
  # main
  # ----------------------------------------------------------------------------
  ds <- info$data$unscaled %>%
    select(-contains('I('))
  if (is.null(info$vars$m) & type == 'ss') {
    return(ds)
  }
  for (j in 1:ncol(info$grid[[type]])) {
    temp <- list()
    temp$var <- colnames(info$grid[[type]])[j]
    temp$value <- info$grid[[type]][i, j]
    if (temp$var == info$vars$x$name) {
      ds[, 'x_pd'] <- ds[, info$vars$x$name] - temp$value
      ds[, 'x_pd_sq'] <- ds[, 'x_pd']^2
    } else {
      if (info$vars$m[[temp$var]]$type == 'factor') {
        ds[, temp$var] <- as.factor(ds[, temp$var])
        temp$levels <- info$vars$m[[temp$var]]$levels
        contrasts(ds[, temp$var]) <- contr.treatment(length(temp$levels), base = which(temp$value == temp$levels))
      } else if (info$vars$m[[temp$var]]$type == 'numeric') {
        ds[, temp$var] <- ds[, temp$var] - temp$value
      }
    }
  }
  return(ds)

}
