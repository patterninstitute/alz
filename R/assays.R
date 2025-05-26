#' Alzheimer's disease assays
#'
#' @description
#'
#' Returns a tibble containing curated metadata for assay implementations used
#' to measure fluid or imaging biomarkers relevant to Alzheimer's disease
#' research. Each row describes a specific combination of vendor, platform, and
#' technology used to quantify a given analyte.
#'
#' Assays represent product-specific implementations (e.g., Elecsys, Simoa HD-X,
#' INNOTEST) that produce numeric measurements for downstream biomarker
#' classification. Quantitative performance parameters such as lower and upper
#' limits of quantification (LLOQ, ULOQ) are included where available.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{`assay_id`}{Unique identifier for the assay.}
#'   \item{`analyte_id`}{Reference to the measured analyte (see [analytes()]).}
#'   \item{`modality`}{Either `"fluid"` or `"imaging"`, denoting the type of measurement.}
#'   \item{`matrix`}{Biological specimen used, such as `"CSF"`, `"plasma"`, or `"brain"`.}
#'   \item{`cv`}{Coefficient of variation.}
#'   \item{`lloq`}{Lower limit of quantification, in appropriate units.}
#'   \item{`uloq`}{Upper limit of quantification.}
#'   \item{`unit`}{Measurement unit (e.g., `"pg/mL"`, `"SUVR"`).}
#'   \item{`technology`}{Underlying method used (e.g., `"ELISA"`, `"PET"`, `"Simoa"`).}
#'   \item{`vendor`}{Manufacturer of the assay.}
#'   \item{`platform`}{Commercial platform or product line name.}
#'   \item{`description`}{Descriptive label for the assay.}
#' }
#'
#' @seealso [analytes()], [biomarkers()]
#'
#' @examples
#' assays()
#'
#' @export
assays <- function() {
  path <- dataset_path("assays")
  readr::read_rds(file = path)
}
