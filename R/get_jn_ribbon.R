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
#' @examples
get_jn_ribbon <- function(fig, data) {
  temp <- list()
  temp$sig_pos <- data %>%
    filter(sig == 1,
           b > 0)
  temp$sig_neg <- data %>%
    filter(sig == 1,
           b < 0)
  temp$ns <- data %>%
    filter(sig == 0) %>%
    mutate(diff = x - lag(x),
           diff = ifelse(is.na(diff), 0, diff))
  idx <- which(temp$ns$diff > 1)

  # ns
  if (length(idx) == 0) {
    fig <- fig +
      geom_ribbon(aes(ymin = b_ci_95_ll, ymax = b_ci_95_ul), data = temp$ns, alpha = 0.25)
  } else {
    fig <- fig +
      geom_ribbon(aes(ymin = b_ci_95_ll, ymax = b_ci_95_ul), data = temp$ns[1:(idx-1), ], alpha = 0.1) +
      geom_ribbon(aes(ymin = b_ci_95_ll, ymax = b_ci_95_ul), data = temp$ns[(idx):nrow(temp$ns), ], alpha = 0.1)
  }

  # sig
  if (nrow(temp$sig_neg) > 0) {
    fig <- fig +
      geom_ribbon(aes(ymin = b_ci_95_ll, ymax = b_ci_95_ul), data = temp$sig_neg, alpha = 0.5)
  }
  if (nrow(temp$sig_pos) > 0) {
    fig <- fig +
      geom_ribbon(aes(ymin = b_ci_95_ll, ymax = b_ci_95_ul), data = temp$sig_pos, alpha = 0.5)
  }
  return(fig)
}
