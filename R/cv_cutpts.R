#' Adjust cut-points using assay precision
#'
#' @description
#'
#' Adjusts one or two diagnostic cut-points using a coefficient of variation
#' (CV) to account for measurement uncertainty. When a single cut-point is
#' provided, it is expanded into a confidence interval based on the expected
#' assay precision. When two cut-points are given (e.g. for defining an
#' indeterminate or equivocal zone), each is adjusted independently: the lower
#' cut-point defines the lower bound, and the upper cut-point defines the upper
#' bound of the resulting interval. This is useful when applying classification
#' rules to continuous biomarker values under analytical variability.
#'
#' @param cutpts A numeric vector of length 1 or 2, representing cut-point(s).
#' @param cv A positive scalar: coefficient of variation to apply.
#' @param dist A character scalar, either `"normal"` or `"lognormal"`, indicating
#'   the distributional assumption used when applying the CV.
#' @param level Confidence level to use (default = 0.95).
#'
#' @returns An updated `cutpts` object.
#'
#' @examples
#' cv_cutpts(cutpts(100), cv = 0.1)
#' cv_cutpts(cutpts(c(90, 110)), cv = 0.1)
#'
#' @export
cv_cutpts <- function(cutpts, cv, dist = c("lognormal", "normal"), level = 0.95) {

  if (isFALSE(is_cutpts(cutpts))) {
    stop("`cutpts` must be `cutpts` object.")
  }

  dist <- match.arg(dist)
  direction <- attr(cutpts, "direction", exact = TRUE)

  # If one cut-point
  if (length(cutpts) == 1) {
    new_cutpts <- ci(cutpts, cv = cv, dist = dist, level = level)[1, ]
    return(cutpts(x = new_cutpts, direction = direction))
  }

  # If two cut-points
  ci_matrix <- ci(cutpts, cv = cv, dist = dist, level = level)

  lower <- ci_matrix[1, 1]
  upper <- ci_matrix[2, 2]
  cutpts(c(lower, upper), direction = direction)

}
