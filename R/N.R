#- 1. N2018: combine N biomarkers under the 2018 scheme ------------------

#' N biomarkers (2018 scheme)
#'
#' @description
#' Computes the overall N-status by OR-combining any subset of these biomarkers:
#'   - nfl
#'   - mri_atrophy
#'   - fdg_pet
#' At least one non-empty biomarker must be supplied. Zero-length inputs are ignored.
#'
#' @param nfl         bmk vector for Neurofilament light chain (optional)
#' @param mri_atrophy bmk vector for MRI-derived atrophy (optional)
#' @param fdg_pet     bmk vector for FDG PET (optional)
#'
#' @return A `bmk` vector representing the combined N-status.
#' @keywords internal
N2018 <- function(nfl,
                  mri_atrophy,
                  fdg_pet) {

  # 1. Collect only the arguments that were actually supplied
  supplied <- list()
  if (!missing(nfl))         supplied$nfl         <- nfl
  if (!missing(mri_atrophy)) supplied$mri_atrophy <- mri_atrophy
  if (!missing(fdg_pet))     supplied$fdg_pet     <- fdg_pet

  # 2. Error if none were supplied
  if (length(supplied) == 0L) {
    stop("At least one N-biomarker must be provided for N2018()", call. = FALSE)
  }

  # 3. Coerce each to bmk() and drop any that are zero-length
  bmk_list <- lapply(supplied, function(x) {
    vec <- bmk(x)
    if (length(vec) == 0L) {
      return(NULL)
    }
    vec
  })
  bmk_list <- bmk_list[!vapply(bmk_list, is.null, logical(1))]

  # 4. If, after dropping zero-length, none remain, error
  if (length(bmk_list) == 0L) {
    stop("At least one non-empty N-biomarker must be provided for N2018()", call. = FALSE)
  }

  # 5. Reduce via Kleene's OR (with "-" or "+" dominance over NA)
  Reduce(`|`, bmk_list)
}


#- 2. N(): dispatch based on classification version ----------------------

#' Combine N biomarkers
#'
#' @param nfl         bmk for Neurofilament light chain (optional)
#' @param mri_atrophy bmk for MRI-derived atrophy (optional)
#' @param fdg_pet     bmk for FDG PET (optional)
#' @param .classification Either "2018" (default) or eventually "2024", etc.
#'
#' @return A `bmk` vector representing combined N-status under selected scheme.
#'
#' @examples
#' N(nfl = "+")
#'
#' N(mri_atrophy = "+")
#'
#' N(nfl = "+", mri_atrophy = "+")
#'
#' @export
N <- function(nfl,
              mri_atrophy,
              fdg_pet,
              .classification = c("2018", "2024")) {

  .classification <- match.arg(.classification)
  if (.classification == "2018") {
    N2018(
      nfl         = nfl,
      mri_atrophy = mri_atrophy,
      fdg_pet     = fdg_pet
    )
  } else {
    stop("2024 implementation not provided yet", call. = FALSE)
  }
}
