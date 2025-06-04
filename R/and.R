#' Kleene logical AND: strong or weak
#'
#' @param x,y Logical vectors (will be recycled if lengths differ).
#' @param kleene_logic One of "strong" (default) or "weak".
#'   - "strong": R’s default three-valued AND; FALSE dominates.
#'   - "weak": propagate NA whenever either operand is NA.
#'
#' @return A logical vector of length max(length(x), length(y)).
#'
#' @examples
#' # And operation with strong Kleene logic (default), same as `&`.
#' and(c(TRUE, FALSE, FALSE, NA), c(TRUE, TRUE, NA, NA))
#'
#' # Weak Kleene logic (propagate `NA` even if other is `FALSE`).
#' and(c(TRUE, FALSE, FALSE, NA), c(TRUE, TRUE, NA, NA), kleene_logic = "weak")
#'
#' @export
and <- function(x, y, kleene_logic = c("strong", "weak")) {
  kleene_logic <- match.arg(kleene_logic)
  if (!is.logical(x) || !is.logical(y)) {
    cli::cli_abort("`x` and `y` must be logical vectors.")
  }
  # recycle to common length
  n <- vctrs::vec_size_common(x, y)
  x <- vctrs::vec_recycle(x, n)
  y <- vctrs::vec_recycle(y, n)

  switch(kleene_logic,
         strong = x & y,
         weak   = ifelse(is.na(x) | is.na(y), NA, x & y))
}
