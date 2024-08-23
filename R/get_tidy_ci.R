#' get_tidy_ci
#'
#' @param model
#'
#' @return
#' @export
#' @importFrom broom tidy
#' @examples
get_tidy_ci <- function(model) {
  df_tidy <- tidy(model) %>%
    rename(rh = term,
           b = estimate,
           se = std.error,
           t = statistic,
           p = p.value)
  df_ci <- confint(model)
  colnames(df_ci) <- c('b_ci_95_ll', 'b_ci_95_ul')
  df_tidy <- cbind(df_tidy, df_ci)
  rownames(df_tidy) <- NULL
  return(df_tidy)
}

#' get_tidy_ci_mod
#'
#' @param info
#' @param model
#' @param i
#' @param type
#'
#' @return
#' @export
#' @import dplyr
#' @examples
get_tidy_ci_mod <- function(info, model, i, type) {
  df_tidy <- get_tidy_ci(model)
  if (is.null(info$grid[[type]])) {
    return(df_tidy)
  }
  for (j in 1:ncol(info$grid[[type]])) {
    if (colnames(info$grid[[type]])[j] == info$vars$x$name) {
      df_tidy[, 'x'] <- info$grid[[type]][i, j]
    } else {
      mj <- which(colnames(info$grid[[type]])[j] == names(info$vars$m))
      df_tidy[, glue('m{mj}')] <- info$grid[[type]][i, j]
    }
  }
  df_tidy <- df_tidy %>%
    select(contains('x'), contains('m'), everything())
  return(df_tidy)
}
