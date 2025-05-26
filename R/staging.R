bio_stages_with_imaging_biomarkers <- function() {
  tibble::tibble(
    bio_stage_id = c("A", "B", "C", "D"),
    bio_stage_scheme = "imaging",
    bio_stage_name = c("initial", "early", "intermediate", "advanced"),
    bio_stage_shrt_desc = c(
      "Amyloid PET",
      "Tau PET medial temporal region",
      "Tau PET moderate neocortical uptake",
      "Tau PET high neocortical uptake"
    ),
    bio_stage_long_desc = c(
      "Abnormal amyloid PET with no uptake on tau PET",
      "Abnormal amyloid PET plus tau PET update that is restricted to medial temporal areas",
      "Abnormal amyloid PET plus tau PET uptake in the moderate standardized uptake value (SUVR) range on a newcortical ROI",
      "Abnormal amyloid PET plus tau PET uptake in the high SUVR range in the same neocortical ROI"
    )
  )
}

bio_stages_with_fluid_biomarkers <- function() {
  tibble::tibble(
    bio_stage_id = c("A", "B", "C", "D"),
    bio_stage_scheme = "fluid",
    bio_stage_name = c("initial", "early", "intermediate", "advanced"),
    bio_stage_shrt_desc = paste0(.data$bio_stage_name, "-stage biomarkers"),
    bio_stage_long_desc = NA
  )
}

bio_stages <- function(modality = c("imaging", "fluid")) {
  modality <- match.arg(modality)

  if (identical(modality, "imaging")) {
    bio_stages_with_imaging_biomarkers()
  } else {
    bio_stages_with_fluid_biomarkers()
  }
}
