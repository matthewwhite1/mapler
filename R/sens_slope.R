#' Calculate the Sen's slope for a numeric vector
#'
#' @param x numeric vector
#' @param conf_level numeric confidence level
#'
#' @return list of five items: Sen's slope estimate,
#'   z statistic, p-value, sample size, and confidence interval
#'
#' @export
sens_slope <- function(x, conf_level = 0.95) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  } else if (length(x) == 1) {
    stop("x must be longer than 1")
  }
  stats::na.fail(x)
  t <- table(x)
  output <- sens_slope_rcpp(x, t, conf_level)
  named_output <- list(
    "estimates" = c("Sen's slope" = output[1]),
    "statistic" = c("z" = output[2]),
    "p.value" = output[3],
    "parameter" = c("n" = output[4]),
    "conf.int" = output[5:6]
  )
  attr(named_output$conf.int, "conf.level") <- conf_level
  named_output
}
