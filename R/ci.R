#' Confidence interval from coefficient of variation
#'
#' [ci()] calculates the confidence interval around observed values given a
#' coefficient of variation (CV).
#'
#' @param x A numeric vector of positive measured values.
#'
#' @param cv A positive scalar: coefficient of variation (e.g., 0.10 for 10%).
#'
#' @param dist A character scalar indicating the distributional assumption used to
#'   compute the confidence interval. Must be one of:
#'
#'   - `"normal"`: assumes measurement errors are normally distributed. The confidence
#'     interval will be symmetric around the observed value.
#'   - `"lognormal"`: assumes measurement errors follow a log-normal distribution.
#'     This results in an asymmetric confidence interval, which is often more appropriate
#'     for strictly positive and right-skewed measurements (e.g., biomarker concentrations).
#'
#' @param level Confidence level as a proportion (e.g., 0.95 for 95%). Default is 0.95.
#'
#' @returns A matrix with two columns (`lower`, `upper`) and one row per element
#'   of `x`.
#'
#' @examples
#' # Lognormal assumption is often more appropriate for strictly positive
#' # and right-skewed measurements (e.g., biomarker concentrations).
#' ci(c(100, 200), 0.10, dist = "lognormal", level = 0.95)
#'
#' # Normal assumption
#' ci(c(100, 200), 0.10, dist = "normal", level = 0.95)
#'
#' @export
ci <- function(x, cv, dist = c("lognormal", "normal"), level = 0.95) {
  stopifnot(
    is.numeric(x), all(x > 0),
    is.numeric(cv), length(cv) == 1, cv > 0,
    is.numeric(level), length(level) == 1, level > 0, level < 1
  )

  dist <- match.arg(dist)
  z <- stats::qnorm(1 - (1 - level) / 2)  # z-score for desired confidence level

  ci <- switch(
    dist,

    "normal" = {
      se <- x * cv
      cbind(
        lower = x - z * se,
        upper = x + z * se
      )
    },

    "lognormal" = {
      sigma_log <- sqrt(log(1 + cv^2))
      cbind(
        lower = x * exp(-z * sigma_log),
        upper = x * exp(z * sigma_log)
      )
    }
  )

  return(ci)
}
