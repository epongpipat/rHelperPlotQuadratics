#' get_ss_plot
#'
#' @param model
#' @param x_var
#' @param m_vars
#'
#' @return
#' @export
#' @concept viz
#' @import ggplot2
#' @examples
get_ss_plot <- function(model, x_var, m_vars = NULL, facet_wrap_opts = list(ncol = 3,
                                                                            scales = 'fixed',
                                                                            labeller = 'label_both')) {
  info <- get_model_info(model, x_var = x_var, m_vars = m_vars)
  # ds_pred <- get_ss_pred_all(model, x_var = x_var, m_vars = m_vars)
  ds_pred <- get_ss_pred_all(info)

  if (is.null(m_vars)) {
    fig <- ggplot(ds_pred, aes(x, y)) +
      geom_ribbon(aes(ymin = y_ll, ymax = y_ul), alpha = 0.5)
  } else if (!is.null(m_vars)) {
    fig <- ggplot(ds_pred, aes(x, y, fill = m1))
      facet_wrap_opts[['facets']] <- str_subset(colnames(ds_pred), 'm[0-9]+')
    fig <- fig + do.call(facet_wrap, facet_wrap_opts)
    fig <- fig + geom_ribbon(aes(ymin = y_ll, ymax = y_ul, alpha = as.factor(sig))) +
      labs(alpha = 'Sig.')
    if (length(unique(ds_pred$sig)) == 1) {
      if (unique(ds_pred$sig) == 1) {
        fig <- fig + scale_alpha_manual(values = 0.5)
      }
    } else {
      fig <- fig + scale_alpha_manual(values = c(0.25, 0.5))
    }

  }

  

  fig <- fig +
        # geom_ribbon(aes(ymin = y_ll, ymax = y_ul), alpha = 0.5) +
        geom_line() +
        theme_classic() +
        theme(strip.background = element_blank()) +
        labs(x = info$vars$x$name,
             y = info$vars$formula$lh,
             fill = names(info$vars$m)[1],
             title = 'Simple Slopes')

      fig <- get_caption_msg(fig, info)


      
      return(fig)
}

