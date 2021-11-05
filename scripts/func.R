# display debugging messages in R (if local)
# and in the console log (if running in shiny)
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}

debug_sprintf <- function(fmt, ...) {
  debug_msg(sprintf(fmt, ...))
}

within_t <- function(n = 20, m1 = 0, m2 = 0, sd1 = 1, sd2 = 1, r = 0,
                     alternative = c("two.sided", "less", "greater")) {
  x <- faux::rnorm_multi(n = n,
                         mu = c(A = m1, B = m2),
                         sd = c(sd1, sd2),
                         r = r,
                         empirical = TRUE)

  test <- t.test(x$A, x$B,
                 paired = TRUE,
                 alternative = match.arg(alternative))

  # test info
  list(
    diff_mean = m2 - m1,
    diff_sd = sd(x$A - x$B),
    t = test$statistic[[1]],
    p = test$p.value
  )
}

## solve_r ----
solve_r <- function(param = c("p", "t", "diff_sd"),
                    reported = 0.05,
                    n = 20, m1 = 0, m2 = 0, sd1 = 1, sd2 = 1,
                    alternative = c("two.sided", "less", "greater"),
                    tol = .001) {

  alternative <- match.arg(alternative)
  param <- match.arg(param)

  f <- function(r) {
    sol <- within_t(n, m1, m2, sd1, sd2, r, alternative)
    abs(sol[[param]] - reported)
  }

  opt <- optimise(f, interval = c(-.99, .99))

  # check if solved
  if (opt$objective > tol) {
    return(NULL)
  } else {
    opt$minimum
  }
}

## check_values ----
check_values <- function(n, m1, sd1, m2, sd2) {
  r_min <- -.99
  r_max <- .99
  r_digits <- 2
  alternative <- c("less", "greater", "two.sided")

  # set up params
  tidyr::crossing(
    n = n,
    m1 = m1,
    m2 = m2,
    sd1 = sd1,
    sd2 = sd2,
    r = seq(r_min, r_max, 10^(-r_digits)),
    alternative = alternative
  ) %>%
    # calculate p and t-values
    dplyr::bind_cols(purrr::pmap_df(., within_t))
}

## r_plot ----

r_plot <- function(r_table, reported_p = NULL) {
  p <- ggplot(r_table, aes(x = r, y = p, color = alternative))
  if (!is.na(reported_p)) {
    p <- p + geom_hline(yintercept = reported_p, alpha = 0.5)
  }
  p + geom_line(size = 1, alpha = 0.8)
}

## t_plot ----

t_plot <- function(r_table, reported_t = NULL) {
  t <- ggplot(r_table, aes(x = r, y = t))
  if (!is.na(reported_t)) {
    t <- t + geom_hline(yintercept = reported_t, alpha = 0.5)
  }
  t + geom_line(size = 1, color = "purple", alpha = 0.8)
}
