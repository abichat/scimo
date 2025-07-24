#' Decide which variable to keep
#'
#' @param values A numeric vector, with one value per variable to keep or
#' discard.
#' @param n_kept Number of variables to keep.
#' @param prop_kept A numeric value between 0 and 1 representing the proportion
#' of variables to keep. `n_kept` and `prop_kept` are mutually exclusive.
#' @param cutoff Threshold beyond which (below or above) the variables are
#' discarded.
#' @param maximize Whether to minimize (`FALSE`) or maximize (`TRUE`, the
#' default) the quantity given by `values`.
#'
#' @return A logical vector indicating if variables are kept or discarded.
#'
#' @importFrom rlang abort
#'
#' @author Antoine Bichat
#'
#' @keywords internal
#'
#' @examples
#' scimo:::var_to_keep(1:5, n_kept = 3, maximize = TRUE)
#' scimo:::var_to_keep(1:10, cutoff = 8, maximize = FALSE)
var_to_keep <- function(
  values,
  n_kept = NULL,
  prop_kept = NULL,
  cutoff = NULL,
  maximize = TRUE
) {
  if (!is.null(n_kept) & !is.null(prop_kept)) {
    abort("`n_kept` and `prop_kept` are mutually exclusive.")
  } else if (!is.null(n_kept)) {
    if (maximize) {
      to_keep <- rank(-values, ties.method = "min") <= n_kept
    } else {
      to_keep <- rank(values, ties.method = "min") <= n_kept
    }
  } else if (!is.null(prop_kept)) {
    n_from_prop <- prop_kept * length(values)

    if (maximize) {
      to_keep <- rank(-values, ties.method = "min") <= n_from_prop
    } else {
      to_keep <- rank(values, ties.method = "min") <= n_from_prop
    }
  } else if (is.null(n_kept) & is.null(prop_kept)) {
    to_keep <- rep(TRUE, length(values))
  }

  if (!is.null(cutoff)) {
    if (maximize) {
      to_keep[values < cutoff] <- FALSE
    } else {
      to_keep[values > cutoff] <- FALSE
    }
  }

  to_keep
}
