#' An enumeration of bin labels for positivity status
#'
#' @description
#'
#' [bins()] returns a character vector representing the levels used to classify
#' biomarker values into diagnostic bins:
#'
#' - `"-"`, for negative / unimpaired / normal status
#' - `"~"`, for indeterminate / borderline / uncertain status
#' - `"+"`, for positive / impaired / abnormal status
#'
#' @param n Number of levels to be returned. Use `n = 2` for binary
#'   classification (negative and positive). Use `n = 3` to also include the
#'   intermediate category (`"~"`).
#'
#' @return A character vector of length 2 or 3, representing diagnostic bin
#'   labels.
#'
#' @examples
#' # Use `n = 2` for normal (`"-"`) and abnormal (`"+"`)
#' bins(n = 2)
#'
#' # Use `n = 3` to also include the indeterminate category (`"~"`)
#' bins(n = 3)
#'
#' @export
bins <- function(n = 2) {
  n <- as.integer(n)
  if(n %notin% 2:3) {
    stop("`n` must be either 2 or 3.", call. = FALSE)
  }
  if(identical(n, 2L)) c("-", "+") else c("-", "~", "+")
}

#' Categorize biomarker values into positivity bins
#'
#' [bin_by()] converts numeric biomarker values to diagnostic categories using
#' one or two cut-points. Values equal to a cut-point are assigned to the
#' *higher* bin (i.e., positive or indeterminate), consistent with clinical
#' thresholding practices.
#'
#' @param x A numeric vector of biomarker values.
#'
#' @param cutpts A numeric vector of one or two strictly increasing
#'   cut-points.
#'
#' @return A factor with levels `"-"`, `"+"`, or `"-"`, `"~"`, `"+"` depending
#'   on the number of cutpts.
#'
#' @examples
#' # Using one cut-point
#' bin_by(x = 0:10, cutpts = 5)
#'
#' # Using two cut-points
#' bin_by(x = 0:10, cutpts = c(5, 7))
#'
#' # Using cutpoints from the curated table `cutpoints()`
#' bin_by(x = c(800, 1050, 1300), cutpts = cutpts("ab42-csf-elecsys-willemse2018"))
#'
#' @export
bin_by <- function(x, cutpts) {

  if (isFALSE(is.numeric(x))) {
    stop("`x` must be a numeric vector.")
  }

  if (anyNA(cutpts)) {
    stop("`cutpts` cannot contain missing (`NA`) values.")
  }

  n_cutpts <- length(cutpts)
  if (n_cutpts %notin% 1:2) {
    stop(
      "Number of cut-points must be either one or two. Length of `cutpts` is ",
      n_cutpts,
      call. = FALSE
    )
  }

  cutpts <- sort(cutpts)
  cut(
    x,
    breaks = c(-Inf, cutpts, Inf),
    labels = bins(n = n_cutpts + 1L),
    right = FALSE
  )
}

