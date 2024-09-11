#' get_jn_ribbon
#'
#' @param fig
#' @param data
#'
#' @return
#' @export
#' @concept viz
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_subset
#' @examples
get_jn_ribbon <- function(fig, data) {
  m_vars <- colnames(data) %>% str_subset('m')
  df <- NULL
  c <- 1
  if (length(m_vars) == 0) {
    df <- data %>%
      arrange(x)
    for (j in 1:nrow(df)) {
      if (j == 1) {
        df$segment[j] <- c
        next()
      }
      if (df$sig[j] != df$sig[j-1]) {
        c <- c + 1
      }
      df$segment[j] <- c
    }
  } else {
    for (m_key in m_vars) {
      for (m_value in unique(data[, m_key])) {
        df_temp <- data[data[, m_key] == m_value, ]
        for (j in 1:nrow(df_temp)) {
          if (j == 1) {
            df_temp$segment[j] <- c
            next()
          }
          if (df_temp$sig[j] != df_temp$sig[j-1]) {
            c <- c + 1
          }
          df_temp$segment[j] <- c
        }
        c <- c + 1
        df <- rbind(df, df_temp)
      }
    }
  }
  for (i in 1:length(unique(df$segment))) {
    df_temp <- df %>%
      filter(segment == i)
    if (df_temp$sig[1]) {
      fig <- fig +
        geom_ribbon(aes(ymin = b_ci_95_ll, ymax = b_ci_95_ul), data = df_temp, alpha = 0.5)
    } else {
      fig <- fig +
        geom_ribbon(aes(ymin = b_ci_95_ll, ymax = b_ci_95_ul), data = df_temp, alpha = 0.25)
    }
  }
  return(fig)
}

