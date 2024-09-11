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
get_model_info <- function(model, x_var = NULL, m_vars = NULL, x_step = .01, round = 3) {

  vars <- list()
  data <- list()
  ds <- model$model
  ds_unscaled <- unscale_data(ds)
  data <- list(original = ds,
               unscaled = ds_unscaled)

  # check
  if (!sum(str_detect(colnames(ds_unscaled), x_var))) {
    stop(glue('x_var ({x_var}) not found in dataset'))
  }

  if (!is.null(m_vars)) {
    for (m_var in m_vars) {
      if (!sum(str_detect(colnames(ds_unscaled), m_var))) {
        stop(glue('m_var ({m_var}) not found in dataset'))
      }
    }
  }

  v <- colnames(ds) %>% str_subset(x_var)
  v_clean <- str_remove_fun_from_term(v)
  v <- v[v_clean == x_var]
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
    str_split('[[:]]|[[*]]') %>%
    unlist() %>%
    str_trim() %>%
    unique() %>%
    str_remove_fun_from_term()
  vars$o$control <- vars$o$control[vars$o$control != x_var]

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

  vars$o$control <- vars$o$control[!(vars$o$control %in% vars$o$int_when_zero)]


  grid <- list()

  grid$jn <- data.frame(x = seq(from = 0, to = 1, by = x_step) * (max(ds_unscaled[, x_var]) - min(ds_unscaled[, x_var])) + min(ds_unscaled[, x_var]))
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
        vars$m[[m_var]]$levels_rounded <- round(vars$m[[m_var]]$levels, round)
        while (length(unique(vars$m[[m_var]]$levels)) != length(unique(vars$m[[m_var]]$levels_rounded))) {
          warning(glue('rounding ({round}) is too conservative for {m_var}, trying the next integer'))
          round <- round + 1
          vars$m[[m_var]]$levels_rounded <- round(vars$m[[m_var]]$levels, round)
          #(original: {paste0(vars$m[[m_var]]$levels, collapse = ",")}; rounded: (original: {paste0(vars$m[[m_var]]$levels_rounded, collapse = ",")}
          # vars$m[[m_var]]$levels_rounded <- formatC(temp$info$vars$m$icvf_right_mni152_parietal_sg_k_mc$levels, format = "e", digits = 2)
        }
        vars$formula$jn <- vars$formula$jn %>%
          str_remove_scale_from_formula(., m_var)
        vars$formula$ss <- vars$formula$ss %>%
          str_remove_scale_from_formula(., m_var)
      } else {
        warning("moderating variable must be of type numeric or factor")
      }
    }
    vars$o$control <- vars$o$control[!(vars$o$control %in% names(vars$m))]
    temp <- purrr::map(vars$m, 'levels')
    temp[[x_var]] <- grid$jn[[x_var]]
    grid$jn <- expand.grid(temp) %>%
      select(all_of(x_var), everything())

    grid$ss <- expand.grid(purrr::map(vars$m, 'levels'))

  }

  opts <- list()
  opts$x_step <- x_step
  opts$round <- round

  info <- list(data = data,
               vars = vars,
               grid = grid,
               opts = opts)
  return(info)
}

