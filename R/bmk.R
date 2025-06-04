# bmk <- function(x = character()) {
#   x <- as.character(x)
#   fac <- factor(x, levels = c("-", "~", "+"))
#   structure(fac, class = c("bmk", "factor"))
# }
#
# #' @export
# is_bmk <- function(x) inherits(x, "bmk")
#
# # The three allowed non‐missing values:
# .bmk_values <- c("-", "~", "+")
#
# # NEGATION:   ¬("-") = "+", ¬("~") = "~", ¬("+") = "-"
# .neg_bmk <- c("-" = "+", "~" = "~", "+" = "-")
#
# # CONJUNCTION (strong Kleene style: "−" dominates; "~" propagates)
# .and_bmk <- matrix(
#   c("-", "-", "-",
#     "-", "~", "~",
#     "-", "~", "+"),
#   nrow = 3, byrow = TRUE,
#   dimnames = list(.bmk_values, .bmk_values)
# )
#
# # DISJUNCTION (dual of ∧)
# .or_bmk <- matrix(
#   c("-", "~", "+",
#     "~", "~", "+",
#     "+", "+", "+"),
#   nrow = 3, byrow = TRUE,
#   dimnames = list(.bmk_values, .bmk_values)
# )
#
# # Look‐up helper that preserves NA
# lookup_mat <- function(tbl, a, b) {
#   # a, b are character vectors of "-", "~", "+", or NA
#   out <- rep(NA_character_, length(a))
#   for (i in seq_along(a)) {
#     if (!is.na(a[i]) && !is.na(b[i])) {
#       idx_a <- match(a[i], .bmk_values)
#       idx_b <- match(b[i], .bmk_values)
#       out[i] <- tbl[idx_a, idx_b]
#     }
#     # else leave out[i] as NA
#   }
#   out
# }
#
# #' @export
# Ops.bmk <- function(e1, e2 = NULL) {
#   op <- .Generic
#
#   # Extract underlying chars (possibly NA)
#   x1 <- as.character(e1)
#
#   # ---- Unary NEGATION ----
#   if (op == "!") {
#     out <- rep(NA_character_, length(x1))
#     not_idx <- !is.na(x1)
#     out[not_idx] <- .neg_bmk[x1[not_idx]]
#     return(bmk(out))
#   }
#
#   # ---- Binary operators ("&" or "|") ----
#   if (!(op %in% c("&", "|"))) {
#     stop("Operator `", op, "` not supported for `bmk`", call. = FALSE)
#   }
#
#   # Coerce second operand if needed
#   if (!is_bmk(e2)) {
#     e2 <- bmk(e2)
#   }
#   x2 <- as.character(e2)
#
#   # Recycle to common length
#   n <- vctrs::vec_size_common(x1, x2)
#   x1 <- rep_len(x1, n)
#   x2 <- rep_len(x2, n)
#
#   out_chr <- switch(op,
#                     "&" = lookup_mat(.and_bmk, x1, x2),
#                     "|" = lookup_mat(.or_bmk,  x1, x2)
#   )
#
#   bmk(out_chr)
# }

#— 1. Class constructor + tester ————————————————————————————————
bmk <- function(x = character()) {

  if (is_bmk(x)) return(x)

  x <- as.character(x)
  valid <- is.na(x) | x %in% c("-", "~", "+")
  if (!all(valid)) {
    stop("`x` must only contain '-', '~', '+', or NA", call. = FALSE)
  }
  fac <- factor(x, levels = c("-", "~", "+"))
  structure(fac, class = c("bmk", "factor"))
}

is_bmk <- function(x) inherits(x, "bmk")

#— 2. Lookup tables —————————————————————————————————————————————
.bmk_values <- c("-", "~", "+")

.neg_bmk <- c("-" = "+", "~" = "~", "+" = "-")

.and_bmk <- matrix(
  c("-", "-", "-",
    "-", "~", "~",
    "-", "~", "+"),
  nrow = 3, byrow = TRUE,
  dimnames = list(.bmk_values, .bmk_values)
)

.or_bmk <- matrix(
  c("-", "~", "+",
    "~", "~", "+",
    "+", "+", "+"),
  nrow = 3, byrow = TRUE,
  dimnames = list(.bmk_values, .bmk_values)
)


and_lookup <- function(a, b) {
  n <- length(a)
  out <- character(n)
  for (i in seq_len(n)) {
    ai <- a[i]; bi <- b[i]
    # If either side is exactly "-", result is "-"
    if (!is.na(ai) && ai == "-") {
      out[i] <- "-"
    } else if (!is.na(bi) && bi == "-") {
      out[i] <- "-"
    } else if (is.na(ai) || is.na(bi)) {
      out[i] <- NA_character_
    } else {
      idx_a <- match(ai, .bmk_values)
      idx_b <- match(bi, .bmk_values)
      out[i] <- .and_bmk[idx_a, idx_b]
    }
  }
  out
}

or_lookup <- function(a, b) {
  n <- length(a)
  out <- character(n)
  for (i in seq_len(n)) {
    ai <- a[i]; bi <- b[i]
    # If either side is exactly "+", result is "+"
    if (!is.na(ai) && ai == "+") {
      out[i] <- "+"
    } else if (!is.na(bi) && bi == "+") {
      out[i] <- "+"
    } else if (is.na(ai) || is.na(bi)) {
      out[i] <- NA_character_
    } else {
      idx_a <- match(ai, .bmk_values)
      idx_b <- match(bi, .bmk_values)
      out[i] <- .or_bmk[idx_a, idx_b]
    }
  }
  out
}



#' @export
Ops.bmk <- function(e1, e2 = NULL) {
  op <- .Generic
  x1 <- as.character(e1)

  # Unary negation
  if (op == "!") {
    out <- rep(NA_character_, length(x1))
    not_idx <- !is.na(x1)
    out[not_idx] <- .neg_bmk[x1[not_idx]]
    return(bmk(out))
  }

  # Only "&" or "|" supported
  if (!(op %in% c("&", "|"))) {
    stop("Operator `", op, "` not supported for `bmk`", call. = FALSE)
  }

  # Coerce e2 if necessary
  if (!is_bmk(e2)) {
    e2 <- bmk(e2)
  }
  x2 <- as.character(e2)

  # Recycle
  n <- vctrs::vec_size_common(x1, x2)
  x1 <- rep_len(x1, n)
  x2 <- rep_len(x2, n)

  # Delegate to the chosen helper
  out_chr <- switch(op,
                    "&" = and_lookup(x1, x2),
                    "|" = or_lookup(x1, x2)
  )

  bmk(out_chr)
}
