library(readODS)
library(readr)

biomarkers <- readxl::read_xls(path = "data-raw/biomarkers.xls") |>
  dplyr::mutate(analyte_ids = strsplit(analyte_ids, ","))
readr::write_rds(x = biomarkers, file = "inst/alz-datasets/biomarkers.rds", compress = "xz")
