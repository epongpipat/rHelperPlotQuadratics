#' get_plot
#'
#' @param model
#' @param x_var
#' @param m_vars
#' @param type
#'
#' @return
#' @export
#' @import ggplot2
#' @import patchwork
#' @concept viz
#' @examples
get_plot <- function(model, x_var, m_vars = NULL, type = c('ss', 'jn')) {
  figs <- list()
  if ('ss' %in% type) {
    figs[['ss']] <- get_ss_plot(model, x_var = x_var, m_vars = m_vars) +
      scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10))
  }
  if ('jn' %in% type) {
    figs[['jn']] <- get_jn_plot(model, x_var = x_var, m_vars = m_vars) +
      scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10))
  }
  fig <- wrap_plots(figs, ncol = 1) +
    plot_layout(guides = 'collect')
  return(fig)
}
