#' get_jn_slopes
#'
#' @param model
#' @param x_var
#' @param m_vars
#'
#' @return
#' @export
#' @import dplyr
#' @examples
get_jn_slopes <- function(info) {
  df_tidy <- get_interaction_breakdown(info, type = 'jn')[['jn']] %>%
    mutate(sig = case_when(
      b_ci_95_ll < 0 & b_ci_95_ul < 0 ~ 1,
      b_ci_95_ll > 0 & b_ci_95_ul > 0 ~ 1,
      TRUE ~ 0
    )) %>%
    select(contains('x'), contains('m'), b, sig, b_ci_95_ll, b_ci_95_ul)
  return(df_tidy)
}
