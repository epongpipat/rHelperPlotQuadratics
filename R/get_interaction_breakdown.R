#' get_interaction_breakdown
#'
#' @param model
#' @param x_var
#' @param m_vars
#' @param type
#'
#' @return
#' @export
#' @importFrom abind abind
#' @import dplyr
#' @examples
get_interaction_breakdown <- function(model, x_var, m_vars = NULL, type = c('ss', 'jn')) {
  info <- get_model_info(model, x_var = x_var, m_vars = m_vars)
  models <- list()
  df_tidy <- list()
  data <- list()
  if ('ss' %in% type) {
    models[['ss']] <- get_models(info, type = 'ss')
    df_tidy[['ss']] <- list()
    for (i in 1:length(models[['ss']])) {
      df_tidy[['ss']][[i]]  <- get_tidy_ci_mod(info, models[['ss']][[i]], i, 'ss')
    }
    data[['ss']] <- abind(df_tidy[['ss']], along = 1) %>%
      as.data.frame() #%>%
      # filter(rh == info$vars$x$p2)
    for (j in 1:ncol(data[['ss']])) {
      data[['ss']][, j] <- as_numeric(data[['ss']][, j])
    }
    cat('\nsimple slopes:\n')
    print(data[["ss"]] %>%
      filter(rh == info$vars$x$p2))
  }

  if ('jn' %in% type) {
    models[['jn']] <- get_models(info, type = 'jn')
    df_tidy[['jn']] <- list()
    for (i in 1:length(models[['jn']])) {
      df_tidy[['jn']][[i]] <- get_tidy_ci_mod(info, models[['jn']][[i]], i, 'jn')
    }
    data[['jn']] <- abind(df_tidy[['jn']], along = 1) %>%
      as.data.frame() %>%
      filter(rh == 'x_pd') %>%
      select(-rh)
    for (j in 1:ncol(data[["jn"]])) {
      data[["jn"]][, j] <- as_numeric(data[["jn"]][, j])
    }
    cat('\njohnson-neyman intervals:\n')
    print(get_jn_sig_all(data[['jn']], info))
  }

  return(data)

}
