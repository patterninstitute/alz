#' Retrieve biomarker cut-point values or metadata
#'
#' @description
#'
#' - `cutpoints()`: Provides access to a curated table of biomarker cut-points
#' used to dichotomise or categorise continuous biomarker measurements in
#' clinical or research settings.
#'
#' - `cutpts()` returns only the numeric cut-point(s) associated with that
#' identifier. These thresholds are typically used for classifying biomarker
#' values into diagnostic categories such as positive, negative, or
#' indeterminate, see [cut_by()].
#'
#' @param x A character scalar specifying the identifier of the
#'   cut-point to retrieve, or a numeric vector of cut-point values.
#'
#' @param direction Does positivity increase with `x` (`"increasing"`) or does
#' it decrease with `x` (`"decreasing"`).
#'
#' @returns
#'
#' - For `cutpoints()`: a tibble with columns `cutpt_id`, `bmk_id`,
#'   `cutpts`, `unit`, `assay_id`, `intended_use`, `source`, and `notes`.
#'
#' - For `cutpts()`: numeric value(s) corresponding to the threshold(s) defined
#' for that biomarker-assay combination.
#'
#' @examples
#' # Return the full cut-point table with metadata
#' cutpoints()
#'
#' # One cut-point
#' cutpts(5)
#'
#' # Two cut-points
#' cutpts(c(2, 6))
#'
#' # Two cut-points but increasing values of `x` mean a negative result.
#' cutpts(c(2, 6), "decreasing")
#'
#' # Retrieve the numeric cut-point(s) for a specific identifier
#' cutpts("ab42-csf-elecsys-willemse2018")
#' cutpts("ptau181-ab42-ratio-csf-elecsys-willemse2018")
#'
#' @seealso [cut_by()], [cv_cutpts()]
#'
#' @export
cutpoints <- function() {
  path <- dataset_path("cutpoints")
  readr::read_rds(file = path)
}

new_cutpts <- function(x, direction = c("increasing", "decreasing")) {

  if (isFALSE(is.numeric(x))) {
    stop("`x` must be numeric.", call. = FALSE)
  }

  if (anyNA(x)) {
    stop("cut-point values can't be `NA`.", call. = FALSE)
  }

  n_cutpts <- length(x)
  if (n_cutpts %notin% 1:2) {
    stop(
      "Number of cut-points must be either one or two. Length of `cutpts` is ",
      n_cutpts,
      call. = FALSE
    )
  }

  direction <- match.arg(direction)
  attr(x, "direction") <- direction
  structure(x, class = "cutpts")
}

is_cutpts <- function(x) inherits(x, "cutpts")

#' @export
print.cutpts <- function(x, ...) {
  dir <- attr(x, "direction")
  cps <- unclass(x)
  n <- length(cps)

  plus <- cli::col_red("(+)")
  tilde <- cli::col_yellow("(~)")
  minus <- cli::col_green("(-)")

  if (n == 1L) {
    cp1 <- format(cps[1], trim = TRUE)
    if (dir == "increasing") {
      cat(minus, "<", cp1, "<=", plus, "\n")
    } else {
      cat(plus, "<=", cp1, "<", minus, "\n")
    }
  } else {
    cp1 <- format(cps[1], trim = TRUE)
    cp2 <- format(cps[2], trim = TRUE)
    if (dir == "increasing") {
      cat(minus, "<", cp1, "<=" , tilde, "<", cp2, "<=", plus, "\n")
    } else {
      cat(plus, "<=", cp1, "<", tilde, "<=", cp2, "<", minus, "\n")
    }
  }

  invisible(x)
}

cutpts_from_id <- function(cutpt_id) {
  if (isFALSE(is.character(cutpt_id))) {
    stop("`cutpt_id` must be a character vector.")
  }

  if (length(cutpt_id) != 1L) {
    stop("`cutpt_id` must be a single identifier but has length ",
         length(cutpt_id),
         ".")
  }

  cutpoints <- cutpoints()
  if (cutpt_id %notin% cutpoints$cutpt_id) {
    stop("`cutpt_id` does not match any cut-point identifier.")
  }

  cutpts <-
    cutpoints |>
    dplyr::filter(cutpt_id == !!cutpt_id) |>
    dplyr::select(dplyr::all_of(c("cutpoints", "direction")))

  new_cutpts(x = unlist(cutpts$cutpoints), direction = cutpts$direction)
}

#' @rdname cutpoints
#' @export
cutpts <- function(x,
                   direction = c("increasing", "decreasing")) {
  if (isFALSE(is.character(x) || is.numeric(x))) {
    stop("`x` must be a cut-point id or a numeric vector of cut-point values.")
  }

  if (isTRUE(is.character(x))) {
    cutpts_from_id(cutpt_id = x)
  } else {
    direction <- match.arg(direction)
    new_cutpts(x = x, direction = direction)
  }
}
