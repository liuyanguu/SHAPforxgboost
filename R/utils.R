# Internal helper functions

#' Bins a variable into n_bins quantile groups.
#'
#' Internal function.
#'
#' @importFrom stats quantile
#'
#' @param y Numeric vector to be binned.
#' @param n_bins If \code{y} has up to this number of unique values, no binning is done.
#'
#' @keywords internal
binner <- function(y, n_bins = 7) {
  if (length(unique(y)) <= n_bins) {
    return(y)
  }
  qu <- quantile(y, seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  findInterval(y, unique(qu), rightmost.closed = TRUE)
}

#' Finds variable with presumably strongest interaction effect.
#'
#' Internal function.
#'
#' @param X0 Data set corresponding to data0.
#' @param Xlong Data set corresponding to data_long.
#'
#' @importFrom data.table ':='
#'
#' @keywords internal
strongest_interaction <- function(X0, Xlong) {
  candidates <- setdiff(unique(Xlong[["variable"]]),
                        X0[["variable"]][1])
  n_candidates <- length(candidates)
  if (n_candidates == 0L) {
    return(NULL)
  }
  var_candidates <- numeric(n_candidates)
  names(var_candidates) <- candidates
  X0 <- data.table::copy(X0)

  # bin x_feature
  X0$x_feature <- binner(X0$x_feature)

  # for each candidate, calculate conditional shap variance
  for (cand in candidates) { # cand <- candidates[1]
    X0$color_feature <- binner(Xlong[variable == cand, rfvalue])
    X0[, cond_mean := mean(value, na.rm = TRUE),
          by = c("x_feature", "color_feature")]
    var_candidates[cand] <- X0[, sum((value - cond_mean)^2, na.rm = TRUE)]
  }
  names(which.min(var_candidates))[1]
}
