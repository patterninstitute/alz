#' @export
assays <- function() {
  path <- dataset_path("assays")
  readr::read_rds(file = path)
}
