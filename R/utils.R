#' is_scaled
#'
#' @param x
#'
#' @return
#' @export
#' @concept util
#' @examples
is_scaled <- function(x) {
  return(as.logical(sum(names(attributes(x)) == 'scaled:scale')))
}

#' is_centered
#'
#' @param x
#'
#' @return
#' @export
#' @concept util
#' @examples
is_centered <- function(x) {
  return(as.logical(sum(names(attributes(x)) == 'scaled:center')))
}

#' as_numeric
#'
#' @param x
#'
#' @return
#' @export
#' @concept util
#' @examples
as_numeric <- function(x) {
  numeric <- sum(is.na(suppressWarnings(as.numeric(x)))) != length(x)
  y <- x
  if (numeric) {
    y <- as.numeric(y)
  }
  return(y)
}

#' remove_scale_from_formula
#'
#' @param formula
#' @param term
#'
#' @return
#' @export
#' @concept util
#' @import stringr
#' @import dplyr
#' @examples
remove_scale_from_formula <- function(formula, term) {
  idx <- list()
  idx$term <- str_locate_all(formula, term)[[1]]
  idx$scale_end <- str_locate_all(formula, "\\)")[[1]]
  idx$scale_end <- idx$scale_end[, "start"]
  old_terms <- NULL
  for (i in 1:nrow(idx$term)) {
    temp <- list()
    temp$start <- idx$term[i, "start"] - 6
    temp$diff <- idx$scale_end - idx$term[i, "end"]
    temp$diff[temp$diff < 0] <- NA
    temp$end <- idx$scale_end[which(temp$diff == min(temp$diff, na.rm = TRUE))]

    temp$term <- str_sub(formula, temp$start, temp$end) %>%
      str_replace("\\(", "\\\\(") %>%
      str_replace("\\)", "\\\\)")
    old_terms <- c(old_terms, temp$term)
  }
  if (length(old_terms) == 0) {
    stop("could not find term")
  }
  new_formula <- formula
  for (i in 1:length(old_terms)) {
    new_formula <- str_replace(new_formula, old_terms[i], term)
  }
  return(new_formula)
}

#' unscale
#'
#' @param x
#' @param uncenter
#' @param unscale
#'
#' @return
#' @export
#' @concept util
#' @examples
unscale <- function(x, uncenter = TRUE, unscale = TRUE) {
  if (!is.numeric(x)) {
    warning("x must be numeric")
    return(x)
  }
  y <- x
  if (unscale & is_scaled(x)) {
    y <- y * attributes(x)[["scaled:scale"]]
  }
  if (uncenter & is_centered(x)) {
    y <- y + attributes(x)[["scaled:center"]]
  }
  return(as.numeric(y))
}

#' unscale_data
#'
#' @param data
#'
#' @return
#' @export
#' @concept util
#' @import dplyr
#' @import stringr
#' @importFrom stringr str_remove
#' @examples
unscale_data <- function(data) {
  for (j in 1:ncol(data)) {
    scaled <- as.logical(sum(str_detect(names(attributes(data[, j])), "scaled")))
    if (scaled) {
      var <- colnames(data)[j] %>%
        str_remove("scale\\(") %>%
        str_remove(", scale = F\\)")
      data[, var] <- unscale(data[, j])
    }
  }
  data <- data %>%
    select(-contains("scale"))
  return(data)
}
