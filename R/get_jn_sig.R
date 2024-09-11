#' get_jn_sig
#'
#' @param data
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom glue glue
#' @examples
get_jn_sig <- function(data) {
  df <- data %>%
    mutate(sig = case_when(
             b_ci_95_ll > 0 & b_ci_95_ul > 0 ~ 1,
             b_ci_95_ll < 0 & b_ci_95_ul < 0 ~ 1,
             TRUE ~ 0
           )) %>%
    arrange(x)
  c <- 1
  for (i in 1:nrow(df)) {
    if (i == 1) {
      df$segment[i] <- 1
      next()
    }
    if (df$sig[i] != df$sig[i-1]) {
      c <- c + 1
    }
    df$segment[i] <- c
  }
  df_sig <- df %>%
    filter(sig == 1)
  if (nrow(df_sig) == 0) {
    return("no significant values")
  }
  ranges <- list()
  for (i in unique(df_sig$segment)) {
    df_temp <- df_sig %>%
      filter(segment == i)
    ranges[[as.character(i)]] <- glue("[{min(df_temp$x)}, {max(df_temp$x)}]")
  }


  if (length(ranges) == 1) {
    range_str <- ranges[[1]]
  } else {
    range_str <- paste(ranges, collapse = " U ")
  }
  return(range_str)
}

#' get_jn_sig_all
#'
#' @param data
#' @param info
#'
#' @return
#' @export
#' @import dplyr
#'
#' @examples
get_jn_sig_all <- function(data, info) {

  grid <- info$grid$jn %>%
    select(-info$vars$x$name) %>%
    unique()

  if (nrow(grid) == 0) {
    df_sig <- data.frame(range = get_jn_sig(data))
  } else {
    for (j in 1:ncol(grid)) {
      if (is.numeric(grid[, j])) {
        grid[, j] <- round(grid[, j], info$opts$round)
      }
    }
    df_sig <- grid
    for (i in 1:nrow(grid)) {
      data_sub <- data
      for (j in 1:ncol(grid)) {
        idx <- which(data_sub[, glue("m{j}")] == grid[i, j])
        if (length(idx) == 0) {
          cat("[WARN] there are no indices")
        }
        data_sub <- data_sub[idx, ]
      }
      df_sig[i, 'range'] <- get_jn_sig(data_sub)
    }
  }
  return(df_sig)
}

