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
  df_sig <- data %>%
    mutate(x_scale = scale_min_max(x),
           sig = case_when(
      b_ci_95_ll > 0 & b_ci_95_ul > 0 ~ 1,
      b_ci_95_ll < 0 & b_ci_95_ul < 0 ~ 1,
      TRUE ~ 0
    )) %>%
    filter(sig == 1) %>%
    mutate(diff = x_scale - lag(x_scale),
           diff_jump = diff > 0.1)
  if (nrow(df_sig) == 0) {
    return("no significant values")
  }
  if (length(which(df_sig$diff_jump)) > 1) {
    print(df_sig)
    warning('more than one jump')
  }
  if (length(which(df_sig$diff_jump)) == 0) {
    range <- glue("[{min(df_sig$x)}, {max(df_sig$x)}]")
  } else {
    idx <- which(df_sig$diff_jump)
    range <- glue("[{min(df_sig$x)}, {df_sig$x[idx-1]}] U [{df_sig$x[idx]}, {max(df_sig$x)}]")
  }
  return(range)
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
        grid[, j] <- round(grid[, j], 3)
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

