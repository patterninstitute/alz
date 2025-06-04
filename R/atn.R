#' Perform ATN classification
#'
#' At the moment, [ATN()] performs only the ATN 2018 scheme classification.
#'
#' @param A A-status.
#' @param T T-status.
#' @param N N-status.
#' @param classification ATN classification scheme.
#'
#' @returns To-be-documented.
#'
#' @examples
#' (atn_grid <- atn_grid_2018())
#'
#' ATN(A = atn_grid$A, T = atn_grid$T, N = atn_grid$N)
#'
#' @export
ATN <- function(A, T, N, classification = c("2018")) {
  atn_2018(A = A, T = T, N = N)
}
