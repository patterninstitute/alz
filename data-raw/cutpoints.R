library(readODS)
library(readr)
library(dplyr)

cutpoints <- readODS::read_ods(path = "data-raw/cutpoints.ods", col_types = "ccccccccc") |>
  dplyr::mutate(cutpoints = strsplit(cutpoints, split = ",")) |>
  dplyr::mutate(cutpoints = lapply(cutpoints, as.numeric)) |>
  dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))

readr::write_rds(x = cutpoints, file = "inst/alz-datasets/cutpoints.rds", compress = "xz")
