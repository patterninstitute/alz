#' Alzheimer's disease biomarkers
#'
#' Returns a tibble of curated biomarkers relevant to Alzheimer's disease and
#' related neurodegenerative conditions. Each entry represents a measurable
#' biological or imaging marker, including both original analyte-derived
#' biomarkers and computed ratios or derived indicators.
#'
#' Biomarkers are classified according to category (e.g., core, non-specific,
#' co-pathology), modality (fluid, imaging), and 2024's ATN or related
#' subcategory ("A", "T1", "T2", "N", "I", "V", "S"). Both original and derived
#' biomarkers are supported, and links to the underlying analyte(s) are included
#' for provenance.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{`bmk_id`}{Short identifier for the biomarker (e.g., `"ab42"`, `"ptau181_ab42_ratio"`).}
#'   \item{`bmk_name`}{Human-readable label (e.g., `"Aβ42 / Aβ40"`, `"Amyloid PET"`).}
#'   \item{`bmk_category`}{Biomarker classification: `"core"`, `"non-specific"`, or `"co-pathology"`.}
#'   \item{`bmk_subcategory`}{ATN or co-pathology label: `"A"`, `"T1"`, `"T2"`, `"N"`, `"I"`, `"V"`, `"S"`.}
#'   \item{`bmk_modality`}{Measurement domain: `"fluid"`, `"imaging"`, or `"hybrid"`.}
#'   \item{`bmk_type`}{Whether the biomarker is `"original"` (directly measured) or `"derived"` (computed).}
#'   \item{`analyte_ids`}{List of analyte IDs used to compute or define the biomarker.}
#'   \item{`bmk_desc`}{Concise description of biological relevance or measurement rationale.}
#'   \item{`notes`}{Optional comments or implementation notes.}
#' }
#'
#' @seealso [analytes()], [assays()]
#'
#' @examples
#' biomarkers()
#'
#' @export
biomarkers <- function() {
  path <- dataset_path("biomarkers")
  readr::read_rds(file = path)
}
