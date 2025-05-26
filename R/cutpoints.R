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
#' indeterminate, see [bin_by()].
#'
#' @param cutpt_id A character scalar specifying the identifier of the
#'   cut-point to retrieve.
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
#' # Retrieve the numeric cut-point(s) for a specific identifier
#' cutpts("ab42-csf-elecsys-willemse2018")
#' cutpts("ptau181-ab42-ratio-csf-elecsys-willemse2018")
#'
#' @seealso [bin_by()], [cv_cutpts()]
#'
#' @export
cutpoints <- function() {
  path <- dataset_path("cutpoints")
  readr::read_rds(file = path)
}

#' @rdname cutpoints
#' @export
cutpts <- function(cutpt_id) {
  cutpoints <- cutpoints()

  if (missing(cutpt_id))
    return(cutpoints)

  if (isFALSE(is.character(cutpt_id))) {
    stop("`cutpt_id` must be a character vector.")
  }

  if (length(cutpt_id) != 1L) {
    stop("`cutpt_id` must be a single identifier but has length ",
         length(cutpt_id),
         ".")
  }

  if (cutpt_id %notin% cutpoints$cutpt_id) {
    stop("`cutpt_id` does not match any cut-point identifier.")
  }

  cutpoints |>
    dplyr::filter(cutpt_id == !!cutpt_id) |>
    dplyr::pull("cutpoints")
}
