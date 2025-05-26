enum_biological_stages <- function() {
  LETTERS[1:4]
}

enum_clinical_stages <- function() {
  1:6
}

enum_syndromic_stages <- function() {
  c(
    "cognitively unimpaired",
    "subjective cognitive decline",
    "mild cognitive impairment",
    "mild dementia",
    "moderate dementia",
    "severe dementia"
  )
}
