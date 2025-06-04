#' Alzheimer's disease biomarkers
#'
#' @description
#'
#' [biomarkers()] a dataset of curated biomarkers relevant to Alzheimer's
#' disease (AD) and related neurodegenerative conditions. Each row corresponds
#' to a defined biomarker, including both directly measured biomarkers and
#' derived indicators such as ratios. Variables include identification of the
#' biomarker (`bmk_id` and `bmk_name`), reference(s) to underpinning analyte(s)
#' (`analyte_ids`), and classification details.
#'
#' @details
#' Biomarkers are organized into a structured, three-tier classification system
#' that reflects their biological role, pathogenic target, and stage in the
#' disease process:
#'
#' - `bmk_category`: the highest level of classification, reflecting the
#' biomarker's overall diagnostic role. Categories include:
#'
#'   - `"core"`: biomarkers of AD neuropathologic change (ADNPC), essential for
#'   diagnosis and staging;
#'
#'   - `"non-specific"`: biomarkers relevant to AD pathophysiology but not
#'   specific to AD;
#'
#'   - `"co-pathology"`: markers that capture common comorbid pathologies
#'   frequently co-occurring with AD.
#'
#' - `bmk_class`: a subdivision of the category level, used to distinguish
#' between earlier- and later-appearing markers in the course of disease
#' progression.
#'
#'    For example:
#'
#'      - `"core 1"` includes A and T1 markers that become abnormal early and
#'      support diagnosis;
#'
#'      - `"core 2"` includes T2 markers that become abnormal later and support
#'      disease staging.
#'
#' - `bmk_subclass`: identifies the specific biological process or proteinopathy
#'   the biomarker represents, aligned with the updated ATNISV framework:
#'
#'   - `"A"`: amyloid-beta proteinopathy;
#'   - `"T1"`: soluble tau fragments (e.g., p-tau181, p-tau217, p-tau231);
#'   - `"T2"`: tau aggregates (e.g., tau PET, MTBR-tau243);
#'   - `"N"`: neurodegeneration;
#'   - `"I"`: inflammation and immune activation;
#'   - `"S"`: synucleinopathy;
#'   - `"V"`: vascular brain injury.
#'
#' In contrast, the `bmk_modality` field is not part of the classification
#' hierarchy but denotes the technical measurement domain—either `"fluid"`
#' (e.g., CSF, plasma) or `"imaging"` (e.g., PET, MRI). This
#' distinction is critical because the same biological process (e.g., amyloid
#' pathology) may be captured through multiple modalities, which differ in their
#' temporal resolution, biological specificity, and clinical application. The
#' updated criteria explicitly reject the assumption of equivalence between
#' fluid and imaging biomarkers, hence the need to treat modality separately.
#'
#' @returns A tibble with the following columns:
#' \describe{
#'   \item{`bmk_id`}{Short identifier for the biomarker (e.g., `"ab42"`, `"ptau181_ab42_ratio"`).}
#'   \item{`bmk_name`}{Human-readable label (e.g., `"Aβ42 / Aβ40"`, `"Amyloid PET"`).}
#'   \item{`bmk_category`}{Top-level role: `"core"`, `"non-specific"`, or `"co-pathology"`.}
#'   \item{`bmk_class`}{Stage or use-tier within category (e.g., `"core 1"`, `"core 2"`).}
#'   \item{`bmk_subclass`}{Biological process aligned with the ATNISV framework (e.g., `"A"`, `"T1"`, `"N"`).}
#'   \item{`bmk_modality`}{Measurement domain: `"fluid"` or `"imaging"`.}
#'   \item{`bmk_type`}{Whether the biomarker is `"original"` (directly measured) or `"derived"` (e.g., ratios).}
#'   \item{`analyte_ids`}{List of one or more analyte IDs underpinning the biomarker definition.}
#'   \item{`bmk_desc`}{Short description of what the biomarker measures or represents.}
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
