#' Classify biomarker values based on thresholds and quantification limits
#'
#' [classify()] is a general-purpose function for discretising continuous
#' biomarker values into categorical levels based on user-defined cut-points.
#' It also handles values outside the quantifiable range, using optional
#' labels for values below the lower limit of quantification (LLOQ) or above
#' the upper limit (ULOQ), if provided via names in the `loq` argument.
#'
#' @param x Numeric vector of biomarker values.
#' @param cutpoints Numeric vector of cut-off points. Must be strictly increasing.
#' @param levels Character vector of classification labels, with length equal to `length(cutpoints) + 1`.
#' @param loq Numeric vector of length 2 specifying quantification limits:
#'   lower (LLOQ) and upper (ULOQ). Default: `c(-Inf, Inf)`.
#'   Optionally, you can name the elements to assign labels to values outside the range, e.g.:
#'   `c("Below LLOQ" = 100, "Above ULOQ" = 2000)`.
#' @param right Logical. Intervals are right-closed (i.e. `(a, b]`). Default: `TRUE`.
#' @param include.lowest Logical. Include the lowest value in the first interval. Default: `TRUE`.
#'
#' @return A factor with classification levels, including optional labels for values
#'   outside the quantifiable range.
#'
#' @examples
#' classify(
#'   x = c(10, 75, 130, 2050),
#'   cutpoints = c(50, 150),
#'   levels = c("low", "medium", "high"),
#'   loq = c("Below LLOQ" = 20, "Above ULOQ" = 2000)
#' )
#'
#' @export
classify <- function(x,
                     cutpoints,
                     levels,
                     loq = c(-Inf, Inf),
                     right = TRUE,
                     include.lowest = TRUE) {
  stopifnot(is.numeric(x), is.numeric(cutpoints), is.character(levels))
  stopifnot(length(levels) == length(cutpoints) + 1)

  loq <- sort(loq)
  lloq <- loq[1]
  uloq <- loq[2]

  # Extract labels from loq names
  below_lloq_label <- names(loq)[1]
  above_uloq_label <- names(loq)[2]

  # If names are NULL or empty, set to NA
  if (is.null(below_lloq_label) || below_lloq_label == "") below_lloq_label <- NA
  if (is.null(above_uloq_label) || above_uloq_label == "") above_uloq_label <- NA

  # Track range violations
  below_lloq <- x < lloq
  above_uloq <- x > uloq

  # Mask out-of-quantification values for cut()
  x_in_range <- x
  x_in_range[below_lloq | above_uloq] <- NA

  # Classify using cut()
  out <- cut(
    x_in_range,
    breaks = c(-Inf, sort(cutpoints), Inf),
    labels = levels,
    right = right,
    include.lowest = include.lowest
  )

  # Apply below LLOQ label
  if (!is.na(below_lloq_label)) {
    out <- factor(out, levels = c(below_lloq_label, levels(out)))
    out[below_lloq] <- below_lloq_label
  }

  # Apply above ULOQ label
  if (!is.na(above_uloq_label)) {
    out <- factor(out, levels = c(levels(out), above_uloq_label))
    out[above_uloq] <- above_uloq_label
  }

  return(out)
}


#' Classify biomarker into binary categories using a single threshold
#'
#' @description
#'
#' [classify_binary()] function assigns values to two categories based on a
#' single numeric threshold. Values below the threshold receive the first label
#' in `levels`, and values equal to or above the threshold receive the second
#' label.
#'
#' Values below the lower limit of quantification (`lloq`) or above the upper
#' limit of quantification (`uloq`) are returned as `NA`.
#'
#'
#' @param x Numeric vector of biomarker values.
#' @param cutpoint Single numeric cut-point separating two categories.
#' @param levels Character vector of length 2, specifying labels for values
#'   below and above the threshold, respectively. Defaults to `c("negative",
#'   "positive")`.
#' @param loq Numeric vector of length 2. Limits of quantification: lower and
#'   upper limits. Defaults to `c(-Inf, Inf)` for no limits.
#'
#' @return A factor with two levels, with NA for values outside the quantifiable range.
#'
#' @seealso
#' - [classify_ternary()] for two cut-point categorization, allowing for an indeterminant class.
#' - [classify()] for more flexibility.
#'
#' @export
classify_binary <- function(x,
                            cutpoint,
                            levels = c("negative", "positive"),
                            loq = c(-Inf, Inf)) {
  stopifnot(length(cutpoint) == 1L)
  stopifnot(length(levels) == 2L)
  stopifnot(length(loq) == 2L)

  classify(
    x = x,
    cutpoints = cutpoint,
    levels = levels,
    loq = loq
  )
}

#' Classify biomarker into ternary categories using two thresholds
#'
#' @description
#' This function assigns values to three ordered categories using two numeric
#' thresholds. The default classification is:
#'
#' - Values strictly below lowest of `cutpoints` → `"negative"`
#' - Values between `cutpoints` values → `"indeterminant"`
#' - Values equal to or above highest of `cutpoints` → `"positive"`
#'
#' Interval boundaries are right-closed. Values outside the quantifiable range
#' i.e. below or above `loq` values are returned as `NA`.
#'
#'
#' @param x Numeric vector of biomarker values.
#' @param cutpoints Numeric vector of cut-off points, sorted ascending.
#' @param levels Character vector of length 3. Default is `c("negative",
#'   "indeterminant", "positive")`.
#'
#' @param loq Numeric vector of length 2. Limits of quantification: lower and
#'   upper limits. Defaults to `c(-Inf, Inf)` for no limits.
#'
#' @return A factor with three levels, with `NA` for values outside the
#'   quantifiable range.
#'
#' @seealso
#' - [classify_binary()] for single cut-point categorization.
#' - [classify()] for more flexibility.
#'
#' @export
classify_ternary <- function(x,
                             cutpoints,
                             levels = c("negative", "indeterminant", "positive"),
                             loq = c(lloq = -Inf, uloq = Inf)) {
  stopifnot(length(cutpoints) == 2L)
  stopifnot(length(levels) == 3L)
  stopifnot(length(loq) == 2L)

  classify(
    x = x,
    cutpoints = cutpoints,
    levels = levels,
    loq = loq
  )
}
