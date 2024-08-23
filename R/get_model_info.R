#' get_model_info
#'
#' @param model
#' @param x_var
#' @param m_vars
#' @param x_step
#' @param round
#'
#' @return
#' @export
#' @import stringr
#' @import dplyr
#' @import purrr
#' @examples
get_model_info <- function(model, x_var = NULL, m_vars = NULL, x_step = .1, round = 3) {

  vars <- list()
  data <- list()
  ds <- model$model
  ds_unscaled <- unscale_data(ds)
  data <- list(original = ds,
               unscaled = ds_unscaled)
  v <- colnames(ds) %>% str_subset(x_var)
  vars$x$name <- x_var
  vars$x$p1 <- v[!str_detect(v, "\\^2")]
  vars$x$p2 <- v[str_detect(v, "\\^2")]

  vars$scaled <- list()
  vars$scaled$scaled <- NULL
  vars$scaled$centered <- NULL
  for (i in 1:ncol(ds)) {
    if (is_scaled(ds[, i])) {
      vars$scaled$scaled <- c(vars$scaled$scaled, colnames(ds)[i])
    }
  }
  vars$scaled$scaled <- unique(vars$scaled$scaled)

  for (i in 1:ncol(ds)) {
    if (is_centered(ds[, i])) {
      vars$scaled$centered <- c(vars$scaled$centered, colnames(ds)[i])
    }
  }
  vars$scaled$centered <- unique(vars$scaled$centered)

  vars$formula$lh <- model$terms[[2]] %>% as.character()
  vars$formula$op <- model$terms[[1]] %>% as.character()
  rh_c <- model$terms[[3]] %>% as.character()
  vars$formula$rh <- paste(rh_c[2], rh_c[1], rh_c[3])
  vars$formula$original <- paste(vars$formula$lh, vars$formula$op, vars$formula$rh)

  rh_new <- str_replace(vars$formula$rh, str_replace_all(vars$x$p1, '\\(', '\\\\(') %>% str_replace_all(., '\\)', '\\\\)'), 'x_pd')
  rh_new <- str_replace(rh_new, str_replace_all(vars$x$p2, '\\(', '\\\\(') %>% str_replace_all(., '\\)', '\\\\)') %>% str_replace('\\^', '\\\\^'), 'x_pd_sq')
  vars$formula$jn <- paste(vars$formula$lh, vars$formula$op, rh_new)
  vars$formula$ss <- vars$formula$original

  vars$o$names <- colnames(ds)[!(colnames(ds) %in% c('(Intercept)', vars$formula$lh, vars$x$p1, vars$x$p2))] %>%
    str_remove('scale\\(') %>%
    str_remove(', scale = F\\)')

  vars$o$control <- vars$formula$rh %>%
    str_split('[[+]]') %>%
    unlist() %>%
    str_subset(x_var, negate = TRUE) %>%
    str_trim()

  if (is.null(m_vars)) {
    vars_filter <- c(x_var)
  } else if (!is.null(m_vars)) {
    vars_filter <- c(x_var, m_vars)
  }
  vars$o$int_when_zero <- vars$formula$rh %>%
    str_split('[[+]]') %>%
    unlist() %>%
    str_subset(paste0(vars_filter, collapse = '|'), negate = FALSE) %>%
    str_trim() %>%
    str_split('[[*]]') %>%
    unlist() %>%
    str_subset(paste0(vars_filter, collapse = '|'), negate = TRUE) %>%
    str_trim() %>%
    unique() %>%
    str_remove('scale\\(') %>%
    str_remove(', scale = F\\)')


  grid <- list()
  grid$jn <- data.frame(seq(from = min(ds_unscaled[, x_var]), to = max(ds_unscaled[, x_var]), by = x_step))
  # grid$jn <- data.frame(sort(unique(ds_unscaled[, x_var])))
  colnames(grid$jn) <- x_var

  if (!is.null(m_vars)) {
    vars$m <- list()
    for (m_var in m_vars) {
      vars$m[[m_var]] <- list()
      if (is.factor(ds_unscaled[, m_var])) {
        vars$m[[m_var]]$type <- 'factor'
        vars$m[[m_var]]$levels <- levels(ds[, m_var])
      } else if (is.numeric(ds_unscaled[, m_var])) {
        vars$m[[m_var]]$type <- 'numeric'
        vars$m[[m_var]]$mean <- mean(ds_unscaled[, m_var])
        vars$m[[m_var]]$sd <- sd(ds_unscaled[, m_var])
        vars$m[[m_var]]$levels <- c("-1sd" = vars$m[[m_var]]$mean - vars$m[[m_var]]$sd,
                                    "0sd" = vars$m[[m_var]]$mean,
                                    "+1sd" = vars$m[[m_var]]$mean + vars$m[[m_var]]$sd)
        vars$m[[m_var]]$levels_rounded <- round(c("-1sd" = vars$m[[m_var]]$mean - vars$m[[m_var]]$sd,
                                    "0sd" = vars$m[[m_var]]$mean,
                                    "+1sd" = vars$m[[m_var]]$mean + vars$m[[m_var]]$sd), round)
        vars$formula$jn <- vars$formula$jn %>%
          remove_scale_from_formula(., m_var)
        # update formula
        # probably needs to be more thorough
        vars$formula$ss <- vars$formula$ss %>%
          remove_scale_from_formula(., m_var)
          # str_replace_all(glue("scale\\({m_var}"), m_var) %>%
          # str_replace_all(glue("{m_var}, scale = F\\)"), m_var)

      } else {
        warning("moderating variable must be of type numeric or factor")
      }
    }

    temp <- purrr::map(vars$m, 'levels')
    temp[[x_var]] <- grid$jn[[x_var]]
    grid$jn <- expand.grid(temp) %>%
      select(all_of(x_var), everything())

    grid$ss <- expand.grid(purrr::map(vars$m, 'levels'))

  }



  info <- list(data = data,
               vars = vars,
               grid = grid)
  return(info)
}

