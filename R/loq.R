#' Limits of quantification per method
#'
#' [loq()] is a convenience function that retrieves the limits of quantification
#' from one of the assays in [assays()].
#'
#' @param assay_id A method identifier. One of the values in the `assay_id`
#' variable in [assays()].
#'
#' @returns A named numeric vector of two elements: `lloq`, lower, and `uloq`,
#'   upper, limits of quantification.
#'
#' @examples
#' # Limits of quantification of Fujirebio INNOTEST β-Amyloid(1-42) CSF ELISA
#' # Check `assays()` for available methods and associated identifiers.
#' loq(assay_id = "innotest-ab42-csf")
#'
#' # Likewise, for Beta-Amyloid(1-40)
#' loq(assay_id = "innotest-ab40-csf")
#'
#' @export
loq <- function(assay_id) {
  stopifnot(is.character(assay_id), length(assay_id) == 1L)

  if (is.na(assay_id))
    return(c(lloq = NA_real_, uloq = NA_real_))

  tibble::tibble(assay_id = assay_id) |>
    dplyr::left_join(assays(), by = "assay_id") |>
    dplyr::select(dplyr::all_of(c("lloq", "uloq"))) |>
    unlist()
}
