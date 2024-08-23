#' get_caption_msg
#'
#' @param fig
#' @param info
#'
#' @return
#' @export
#' @concept viz
#' @importFrom stringr str_detect
#' @importFrom glue glue
#' @import ggplot2
#' @examples
get_caption_msg <- function(fig, info) {
  caption_msg <- glue("\nFormula:           {info$vars$formula$original}")

  if (length(info$vars$o$int_when_zero) > 0) {
    temp_vars <- info$vars$o$int_when_zero
    for (i in 1:length(temp_vars)) {
      if (sum(str_detect(info$vars$scaled$centered, temp_vars[i])) == 1) {
        idx <- which(str_detect(info$vars$scaled$centered, temp_vars[i]))
        temp_vars[idx] <- glue("{temp_vars[idx]} (mean)")
      }
    }
    caption_msg <- glue("{caption_msg}\nWhen zero:       {paste0(temp_vars, collapse = ', ')}")
  }

  if (length(info$vars$o$control) > 0) {
    caption_msg <- glue("{caption_msg}\nControlling for:  {paste0(info$vars$o$control, collapse = ', ')}")
  }
  if (caption_msg != "") {
    fig <- fig +
      theme(plot.caption = element_text(hjust = 0)) +
      labs(caption = glue("Note:\n{caption_msg}"))
  }
  return(fig)
}
