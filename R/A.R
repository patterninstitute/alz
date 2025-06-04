#' Combine A biomarkers under the 2018 scheme
#'
#' @description
#' Computes the overall A-status by OR-combining any subset of these biomarkers:
#'   - ab40
#'   - ab42
#'   - ab42_ab40_ratio
#'   - amyloid_pet
#' At least one of these must be supplied. Missing (zero-length) inputs are
#' treated as `NA` to allow proper Kleene OR logic.
#'
#' @param ab40               AB40 as a `bmk` vector (no default; can omit)
#' @param ab42               AB42 as a `bmk` vector (no default; can omit)
#' @param ab42_ab40_ratio    AB42/AB40 ratio as a `bmk` vector (no default; can omit)
#' @param amyloid_pet        Amyloid PET as a `bmk` vector (no default; can omit)
#'
#' @returns A `bmk` vector representing the combined A-status --- one biomarker
#' positive is sufficient to render A-status positive.
#'
#' @keywords internal
A2018 <- function(ab40,
                  ab42,
                  ab42_ab40_ratio,
                  amyloid_pet) {

  # 1. Collect arguments that were actually passed
  supplied <- list()
  if (!missing(ab40))            supplied$ab40            <- ab40
  if (!missing(ab42))            supplied$ab42            <- ab42
  if (!missing(ab42_ab40_ratio)) supplied$ab42_ab40_ratio <- ab42_ab40_ratio
  if (!missing(amyloid_pet))     supplied$amyloid_pet     <- amyloid_pet

  # 2. Error if none were passed
  if (length(supplied) == 0L) {
    stop("At least one A-biomarker must be provided for A2018()", call. = FALSE)
  }

  # 3. Coerce each supplied argument to bmk() and drop any that are zero-length
  bmk_list <- lapply(supplied, function(x) {
    vec <- bmk(x)
    if (length(vec) == 0L) {
      return(NULL)
    }
    vec
  })
  # Remove NULLs (zero-length)
  bmk_list <- bmk_list[!vapply(bmk_list, is.null, logical(1))]

  # 4. If, after dropping zero-length, none remain, error
  if (length(bmk_list) == 0L) {
    stop("At least one non-empty A-biomarker must be provided for A2018()", call. = FALSE)
  }

  # 5. Reduce via Kleene's OR (with "-" dominating NA on AND, "+" dominating on OR)
  Reduce(`|`, bmk_list)
}

#' Combine A biomarkers
#'
#' @description
#' Computes the overall A-status by OR-combining any subset of these biomarkers:
#'
#'   - `ab40`
#'   - `ab42`
#'   - `ab42_ab40_ratio`
#'   - `amyloid_pet`
#'
#' At least one of these must be supplied. Missing (zero-length) inputs are
#' treated as `NA` to allow proper Kleene OR logic.
#'
#' @param ab40 AB40 as a `bmk` or character vector (no default; can omit)
#' @param ab42 AB42 as a `bmk` or character vector (no default; can omit)
#' @param ab42_ab40_ratio AB42/AB40 ratio as a `bmk` or character vector (no default; can omit)
#' @param amyloid_pet Amyloid PET as a `bmk` or character vector (no default; can omit)
#' @param .classification ATN classification scheme.
#'
#' @returns A `bmk` vector representing the combined A-status --- one biomarker
#' positive is sufficient to render A-status positive.
#'
#' @examples
#' A(ab40 = "+")
#'
#' A(ab40 = "+", ab42 = "-")
#'
#' A(ab40 = "-", ab42 = "-")
#'
#' @export
A <- function(ab40 = bmk(),
              ab42 = bmk(),
              ab42_ab40_ratio = bmk(),
              amyloid_pet = bmk(),
              .classification = c("2018", "2024")) {
  .classification <- match.arg(.classification)

  if (identical(.classification, "2024")) {
    stop("2024 implementation not implemented yet!")
  }

  if (identical(.classification, "2018")) {
    A2018(
      ab40 = ab40,
      ab42 = ab42,
      ab42_ab40_ratio = ab42_ab40_ratio,
      amyloid_pet = amyloid_pet
    )
  }
}
