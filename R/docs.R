assay_doc_url <- function(assay_id) {
  base_url <- "https://raw.githubusercontent.com/patterninstitute/alz/main/assay-docs"
  filename <- paste0(assay_id, ".pdf")
  file.path(base_url, filename)
}

#' Open assay reference document
#'
#' `assay_doc()` opens the technical fact sheet associated with a given assay.
#' This is useful for quickly checking specific assay implementations.
#'
#' @param assay_id A character string representing the assay identifier (e.g.
#'   `"innotest-ab42-csf"`). Check [assays()] for options.
#'
#' @returns Called for its side effect. Opens the PDF document in the system
#'   viewer.
#'
#' @examples
#' \dontrun{
#' assay_doc("innotest-ab42-csf")
#' }
#'
#' @export
assay_doc <- function(assay_id) {
  utils::browseURL(url = assay_doc_url(assay_id = assay_id))
  invisible(TRUE)
}

cutpt_doc_url <- function(cutpt_id) {
  base_url <- "https://raw.githubusercontent.com/patterninstitute/alz/main/cutpt-docs"
  filename <- paste0(cutpt_id, ".pdf")
  file.path(base_url, filename)
}

#' Open cut-point reference document
#'
#' `cutpt_doc()` opens the reference document describing the cut-point choice.
#' This is useful for quickly checking the documentation source for cut-points.
#'
#' @param cutpt_id A character string representing the assay identifier (e.g.
#'   `"ab42-csf-elecsys-adni-upenn2013"`). Check [cutpoints()] for options.
#'
#' @returns Called for its side effect. Opens the PDF document in the system
#'   viewer.
#'
#' @examples
#' \dontrun{
#' assay_doc("ab42-csf-elecsys-adni-upenn2013")
#' }
#'
#' @export
cutpt_doc <- function(cutpt_id) {
  utils::browseURL(url = cutpt_doc_url(cutpt_id = cutpt_id))
  invisible(TRUE)
}
