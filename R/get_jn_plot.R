#' get_jn_plot
#'
#' @param model
#' @param x_var
#' @param m_vars
#'
#' @return
#' @export
#' @concept viz
#' @import dplyr
#' @importFrom stringr str_subset
#' @import ggplot2
#' @importFrom glue glue
#' @examples
get_jn_plot <- function(model, x_var, m_vars = NULL, facet_wrap_opts = list(ncol = 3,
                                                                            scales = 'fixed',
                                                                            labeller = 'label_both')) {
  info <- get_model_info(model, x_var = x_var, m_vars = m_vars)
  df_pred <- get_jn_slopes(info)
  grid <- df_pred %>%
    select(contains('m')) %>%
    unique()

  # initialize
  if (is.null(m_vars)) {
    fig <- ggplot(df_pred, aes(x, b))
  } else if (!is.null(m_vars)) {
    fig <- ggplot(df_pred, aes(x, b, fill = m1))
    facet_wrap_opts[['facets']] <- str_subset(colnames(df_pred), 'm')
    fig <- fig + do.call(facet_wrap, facet_wrap_opts)
  }

  # add x-axis
  fig <- fig +
    geom_hline(yintercept = 0, linetype = 'dashed')

  # add ribbon
  if (is.null(m_vars)) {
    fig <- get_jn_ribbon(fig, df_pred)
  } else if (length(m_vars) == 1) {
    for (i in 1:nrow(grid)) {
      temp <- list()
      temp$data <- df_pred %>%
        filter(m1 == grid[i, ])
      fig <- get_jn_ribbon(fig, temp$data)
    }
  } else {
    for (i in 1:nrow(grid)) {
      # cat(i, '\n')
      temp <- list()
      temp$data <- left_join(grid[i, ], df_pred)
      fig <- get_jn_ribbon(fig, temp$data)
    }
  }

  fig <- fig +
    geom_line() +
    theme_classic() +
    theme(strip.background = element_blank()) +
    labs(title = glue('Johnson-Neyman'),
         x = x_var,
         fill = names(info$vars$m)[1],
         y = glue('b ({info$vars$formula$lh} / {x_var})'))
  fig <- get_caption_msg(fig, info)
  return(fig)
}

