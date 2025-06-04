#- 1. T2018: combine all T1 and T2 biomarkers under the 2018 scheme ---------

#' T biomarkers (2018 scheme)
#'
#' @description
#' Computes the overall T-status by OR-combining any subset of these biomarkers:
#'   - t_tau
#'   - ptau217
#'   - ptau181
#'   - ptau231
#'   - ptau181_ab42_ratio
#'   - ptau217_np217_ratio
#'   - ptau205
#'   - mtbr_tau243
#'   - np_tau
#'   - tau_pet
#'   - ttau_ab42_ratio
#' At least one non-empty biomarker must be supplied. Zero-length inputs are ignored.
#'
#' @param t_tau                bmk vector for total tau (optional)
#' @param ptau217              bmk vector for p-tau217 (optional)
#' @param ptau181              bmk vector for p-tau181 (optional)
#' @param ptau231              bmk vector for p-tau231 (optional)
#' @param ptau181_ab42_ratio   bmk vector for p-tau181/AB42 ratio (optional)
#' @param ptau217_np217_ratio  bmk vector for p-tau217/non-p-tau217 ratio (optional)
#' @param ptau205              bmk vector for p-tau205 (optional)
#' @param mtbr_tau243          bmk vector for MTBR-tau243 (optional)
#' @param np_tau               bmk vector for non-phospho tau (optional)
#' @param tau_pet              bmk vector for tau PET (optional)
#' @param ttau_ab42_ratio      bmk vector for total tau/AB42 ratio (optional)
#'
#' @return A `bmk` vector representing the combined T-status.
#' @keywords internal
T2018 <- function(t_tau,
                  ptau217,
                  ptau181,
                  ptau231,
                  ptau181_ab42_ratio,
                  ptau217_np217_ratio,
                  ptau205,
                  mtbr_tau243,
                  np_tau,
                  tau_pet,
                  ttau_ab42_ratio) {

  # 1. Collect only the arguments that were actually supplied
  supplied <- list()
  if (!missing(t_tau))               supplied$t_tau               <- t_tau
  if (!missing(ptau217))             supplied$ptau217             <- ptau217
  if (!missing(ptau181))             supplied$ptau181             <- ptau181
  if (!missing(ptau231))             supplied$ptau231             <- ptau231
  if (!missing(ptau181_ab42_ratio))  supplied$ptau181_ab42_ratio  <- ptau181_ab42_ratio
  if (!missing(ptau217_np217_ratio)) supplied$ptau217_np217_ratio <- ptau217_np217_ratio
  if (!missing(ptau205))             supplied$ptau205             <- ptau205
  if (!missing(mtbr_tau243))         supplied$mtbr_tau243         <- mtbr_tau243
  if (!missing(np_tau))              supplied$np_tau              <- np_tau
  if (!missing(tau_pet))             supplied$tau_pet             <- tau_pet
  if (!missing(ttau_ab42_ratio))     supplied$ttau_ab42_ratio     <- ttau_ab42_ratio

  # 2. Error if none were supplied
  if (length(supplied) == 0L) {
    stop("At least one T-biomarker must be provided for T2018()", call. = FALSE)
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
    stop("At least one non-empty T-biomarker must be provided for T2018()", call. = FALSE)
  }

  # 5. Reduce via Kleene's OR (with "-" or "+" dominance over NA)
  Reduce(`|`, bmk_list)
}


#- 2. T(): dispatch based on classification version ----------------------

#' Combine T biomarkers
#'
#' @param t_tau                bmk for total tau (optional)
#' @param ptau217              bmk for p-tau217 (optional)
#' @param ptau181              bmk for p-tau181 (optional)
#' @param ptau231              bmk for p-tau231 (optional)
#' @param ptau181_ab42_ratio   bmk for p-tau181/AB42 ratio (optional)
#' @param ptau217_np217_ratio  bmk for p-tau217/non-p-tau217 ratio (optional)
#' @param ptau205              bmk for p-tau205 (optional)
#' @param mtbr_tau243          bmk for MTBR-tau243 (optional)
#' @param np_tau               bmk for non-phospho tau (optional)
#' @param tau_pet              bmk for tau PET (optional)
#' @param ttau_ab42_ratio      bmk for total tau/AB42 ratio (optional)
#' @param .classification      Either "2018" (default) or eventually "2024", etc.
#'
#' @return A `bmk` vector representing combined T-status under selected scheme.
#' @export
T <- function(t_tau,
              ptau217,
              ptau181,
              ptau231,
              ptau181_ab42_ratio,
              ptau217_np217_ratio,
              ptau205,
              mtbr_tau243,
              np_tau,
              tau_pet,
              ttau_ab42_ratio,
              .classification = c("2018", "2024")) {

  .classification <- match.arg(.classification)
  if (.classification == "2018") {
    T2018(
      t_tau               = t_tau,
      ptau217             = ptau217,
      ptau181             = ptau181,
      ptau231             = ptau231,
      ptau181_ab42_ratio  = ptau181_ab42_ratio,
      ptau217_np217_ratio = ptau217_np217_ratio,
      ptau205             = ptau205,
      mtbr_tau243         = mtbr_tau243,
      np_tau              = np_tau,
      tau_pet             = tau_pet,
      ttau_ab42_ratio     = ttau_ab42_ratio
    )
  } else {
    stop("2024 implementation not provided yet", call. = FALSE)
  }
}
