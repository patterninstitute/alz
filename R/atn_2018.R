# C.R. Jack Jr. et al. / Alzheimer’s & Dementia 14 (2018) 535-562
atn_profile_2018 <- function(A = character(),
                             T = character(),
                             N = character(),
                             collapse = TRUE,
                             indeterminate_as = NA) {
  A <- bins_as_logical(x = A, indeterminate_as = indeterminate_as)
  T <- bins_as_logical(x = T, indeterminate_as = indeterminate_as)
  N <- bins_as_logical(x = N, indeterminate_as = indeterminate_as)

  na <- dplyr::if_else(collapse, "*", NA_character_)

  A <- dplyr::if_else(A, "+", "-", missing = na)
  T <- dplyr::if_else(T, "+", "-", missing = na)
  N <- dplyr::if_else(N, "+", "-", missing = na)

  if (collapse) {
    sprintf("A%sT%s(N)%s", A, T, N)
  } else {
    tibble::tibble(A, T, N)
  }
}

atn_subcategory_2018 <- function(A = character(),
                                 T = character(),
                                 N = character()) {

  profile <- atn_profile_2018(A = A, T = T, N = N, collapse = FALSE)

  with(
    profile,
    dplyr::case_when(A == "-" & T == "-" & N == "-" ~ "Normal AD biomarkers",
                     A == "+" & T == "-" & N == "-" ~ "Alzheimer's pathologic change",
                     A == "+" & T == "+" & is.na(N) ~ "Alzheimer's disease",
                     A == "+" & T == "+" & N == "-" ~ "Alzheimer's disease",
                     A == "+" & T == "+" & N == "+" ~ "Alzheimer's disease",
                     A == "+" & T == "-" & N == "+" ~ "Alzheimer's and concomitant suspected non Alzheimer's pathologic change",
                     A == "-" & T == "+" & N == "-" ~ "Non-AD pathologic change",
                     A == "-" & T == "-" & N == "+" ~ "Non-AD pathologic change",
                     A == "-" & T == "+" & N == "+" ~ "Non-AD pathologic change",
                     .default = NA_character_)
  )

}

atn_category_2018 <- function(A = character(),
                              T = character(),
                              N = character()) {

  profile <- atn_profile_2018(A = A, T = T, N = N, collapse = FALSE)

  with(
    profile,
    dplyr::case_when(A == "-" & T == "-" & N == "-" ~ "Normal AD biomarkers",
                     A == "+" ~ "Alzheimer's continuum",
                     A == "-" & T == "+" & N == "-" ~ "Non-AD pathologic change",
                     A == "-" & T == "-" & N == "+" ~ "Non-AD pathologic change",
                     A == "-" & T == "+" & N == "+" ~ "Non-AD pathologic change",
                     .default = NA_character_)
  )

}

ad_status_2018 <- function(A = character(),
                           T = character(),
                           indeterminate_as = NA,
                           kleene_logic = c("strong", "weak")) {

  kleene_logic <- match.arg(kleene_logic)

  A <- bins_as_logical(x = A, indeterminate_as = indeterminate_as)
  T <- bins_as_logical(x = T, indeterminate_as = indeterminate_as)

  and(A, T, kleene_logic = kleene_logic)
}

enum_syndromal_stages_2018 <- function(type = c("short", "long")) {

  type <- match.arg(type)

  if (identical(type, "short")) {
    c("CU", "MCI", "DEM")
  } else {
    c(
      "cognitively unimpaired",
      "mild cognitive impairment",
      "dementia"
    )
  }

}

atn_2018 <- function(A = character(),
                     T = character(),
                     N = character()) {
  atn_profile_2018(
    A = A,
    T = T,
    N = N,
    collapse = FALSE
  ) |>
    dplyr::mutate(
      bmk_profile = atn_profile_2018(
        A = A,
        T = T,
        N = N,
        collapse = TRUE
      ),
      bmk_category = atn_category_2018(A = A, T = T, N = N),
      bmk_subcategory = atn_subcategory_2018(A = A, T = T, N = N),
      is_ad = ad_status_2018(A = A, T = T)
    )
}

#' Generate all 2018 ATN combinations
#'
#' [atn_grid_2018()] generates 2018 ATN combinations.
#'
#' @param include_na Whether to include `NA` values.
#'
#' @returns A tibble with columns `A`, `T` and `N`.
#'
#' @export
atn_grid_2018 <- function(include_na = FALSE) {

  levels <- if (include_na) c(NA, "-", "+") else c("-", "+")

  expand.grid(
    N = levels,
    T = levels,
    A = levels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::relocate("A", "T", "N")
}
