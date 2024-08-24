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

#' str_remove_scale_from_formula
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
str_remove_scale_from_formula <- function(formula, term) {
  # formula <- info$vars$formula$ss
  # term <- m_var
  idx <- list()

  idx$term <- str_locate_all(formula, term)[[1]]
  idx$scale_end <- str_locate_all(formula, "\\)")[[1]]
  idx$scale_end <- idx$scale_end[, "start"]
  old_terms <- NULL

  if (length(idx$scale_end) == 0) {
    return(formula)
  }

  for (i in 1:nrow(idx$term)) {
    temp <- list()
    temp$start <- idx$term[i, "start"] - 6
    if (str_sub(formula, temp$start + 1, temp$start + 5) != 'scale') {
      next()
    }

    temp$diff <- idx$scale_end - idx$term[i, "end"]
    temp$diff[temp$diff < 0] <- NA
    temp$end <- idx$scale_end[which(temp$diff == min(temp$diff, na.rm = TRUE))]

    temp$term <- str_sub(formula, temp$start, temp$end) %>%
      str_replace("\\(", "\\\\(") %>%
      str_replace("\\)", "\\\\)")
    old_terms <- c(old_terms, temp$term)
  }
  if (length(old_terms) == 0) {
    return(formula)
    # warning("could not find term")
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

#' str_remove_fun_from_term
#'
#' @param x
#'
#' @return
#' @export
#' @concept util
#' @import stringr
#' @examples
str_remove_fun_from_term <- function(x) {
  y <- x
  start_patterns <- c('\\(')
  ending_patterns <- c(',', '\\^2', '\\)')
  for (i in 1:length(y)) {
    if (str_detect(y[i], start_patterns)) {
      y[i] <- str_sub(y[i], last(str_locate_all(y[i], paste0(start_patterns, collapse = '|'))[[1]][, 'end'])+1, str_length(y[i]))
    }
    if (str_detect(y[i], paste0(ending_patterns, collapse = '|'))) {
      y[i] <- str_sub(y[i], 1, str_locate(y[i], paste0(ending_patterns, collapse = '|'))[1]-1)
    }
    y[i] <- str_trim(y[i])
  }
  return(y)
}

#' scale_min_max
#'
#' @param x
#'
#' @return
#' @export
#' @concept util
#' @examples
scale_min_max <- function(x) {
  y <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  attr(y, 'min') <- min(x, na.rm = TRUE)
  attr(y, 'max') <- max(x, na.rm = TRUE)
  return(y)
}
