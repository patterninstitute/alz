assay_doc_url <- function(assay_id) {
  base_url <- "https://raw.githubusercontent.com/patterninstitute/alz/main"
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
  browseURL(url = assay_doc_url(assay_id = assay_id))
  invisible(TRUE)
}
