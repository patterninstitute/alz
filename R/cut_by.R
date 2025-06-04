bins_as_logical <- function(x, indeterminate_as = NA) {

  if (is.logical(x)) return(x)

  x1 <- rep_len(NA, length.out = length(x))
  x1[x %in% "-"] <- FALSE
  x1[x %in% "~"] <- indeterminate_as
  x1[x %in% "+"] <- TRUE

  x1
}

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
#' [cut_by()] converts numeric biomarker values to diagnostic categories using
#' one or two cut-points. Values equal to a cut-point are assigned to the
#' *higher* bin (i.e., positive or indeterminate), consistent with clinical
#' thresholding practices.
#'
#' @param x A numeric vector of biomarker values.
#'
#' @param cutpts An object of class `cutpts`, see [cutpts()] for more details.
#'
#' @returns A factor with levels `"-"`, `"~"`, `"+"`.
#'
#' @examples
#' # Using one cut-point
#' cut_by(x = 0:10, cutpts = cutpts(5))
#'
#' # Using two cut-points
#' cut_by(x = 0:10, cutpts = cutpts(c(5, 7)))
#'
#' # Using cutpoints from the curated table `cutpoints()`
#' cut_by(x = c(800, 1050, 1300), cutpts = cutpts("ab42-csf-elecsys-willemse2018"))
#'
#' @export
cut_by <- function(x, cutpts) {

  if (isFALSE(is_cutpts(cutpts))) {
    stop("`cutpts` must be `cutpts` object.")
  }

  cutpt_values <- sort(as.numeric(cutpts))
  bins <- bins(n = length(cutpt_values) + 1L)
  direction <- attr(cutpts, "direction", exact = TRUE)
  labels <- if (identical(direction, "increasing")) bins else rev(bins)
  right <- identical(direction, "decreasing")
  cut(
    x,
    breaks = c(-Inf, cutpt_values, Inf),
    labels = labels,
    right = right
  ) |>
    factor(levels = bins(n = 3L)) |>
    bmk()
}

