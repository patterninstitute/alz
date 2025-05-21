#' Alzheimer's disease analytes
#'
#' Returns a tibble containing curated metadata for directly measurable analytes
#' relevant to Alzheimer's disease research. This includes amyloid beta peptides,
#' total tau, phospho-tau isoforms, neurofilament light chain (NfL), GFAP, and
#' alpha-synuclein.
#'
#' The table provides standardized identifiers, human-readable names, molecular
#' types based on NCIT terminology, and concise descriptions of biological relevance.
#' `analyte_id` values are drawn from external ontologies such as ChEBI or UniProt,
#' optionally suffixed to reflect post-translational modifications (e.g. `P10636-pT181`)
#' or structural fragments (e.g. `P10636-MTBR243`).
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{`analyte_id`}{Unique identifier for the analyte. Based on ChEBI or UniProt, with suffixes for specificity.}
#'   \item{`analyte_name`}{Concise scientific label (e.g., `"Aβ42"`, `"p‐tau181"`).}
#'   \item{`analyte_full_name`}{Expanded form of the analyte name.}
#'   \item{`molecular_type`}{NCIT concept code corresponding to the molecular class, e.g. `"peptide"`, `"protein"`, based on NCIT vocabulary.}
#'   \item{`description`}{Brief description of the analyte's biological function or relevance to Alzheimer's disease.}
#' }
#'
#' @seealso [assays()], [biomarkers()]
#'
#' @examples
#' analytes()
#'
#' @export
analytes <- function() {
  path <- dataset_path("analytes")
  readr::read_rds(file = path)
}
