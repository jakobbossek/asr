#' @title Performance measures.
#'
#' @description Performance measures which are frequently used in the field
#' of algorithm selection in both the continuous and discrete domains.
#'
#' @param times [\code{numeric}]\cr
#'   Vector of running times of algorithm runs.
#' @param cutoff [\code{numeric(1)}]\cr
#'   Cutoff time for algorithm runs in seconds. Runs with running times exceeding
#'   the cutoff time are categorized as failed. Default is 1 hour.
#' @param f [\code{numeric(1)}]\cr
#'   Penalty factor. Each failed run is penalized by \code{cutoff} times \code{f}.
#'   Default is 10, which is quite common in the literature on algorithm selection.
#' @param p [\code{numeric(1)}]\cr
#'   Quantile. Default is 0.5.
#' @param score [\code{character(1)}]\cr
#'   Name of score.
#' @param ... [any]\cr
#'   Arguments passed down to score functions.
#' @return [\code{numeric(1)}] Scalar performance value.
#' @rdname performance_measures
#' @name performance_measures
#' @export
#FIXME: add examples
getScore = function(times, score, ...) {
  checkmate::assertNumeric(times, lower = 0, any.missing = FALSE, all.missing = FALSE)
  scores = list(
    "PAR"  = getPARScore,
    "PQR"  = getPQRScore,
    "PERT" = getPERTScore,
    "ERT"  = getPERTScore,
    "SuccessRate" = getSuccessRate,
    "FailureRate" = getFailureRate,
    "RuntimeQuantile" = getRuntimeQuantile
  )
  checkmate::assertChoice(score, names(scores))
  score.fun = scores[[score]]
  do.call(score.fun, c(list(times = times), list(...)))
}

#' @name performance_measures
#' @export
getPARScore = function(times, cutoff = 3600, f = 10, ...) {
  checkmate::assertNumeric(times, lower = 0, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(cutoff, lower = 1)
  checkmate::assertNumber(f, lower = 1)

  idx.timed.out = which(times >= cutoff)
  if (length(idx.timed.out) > 0L)
    times[idx.timed.out] = f * cutoff
  return(mean(times))
}

#' @name performance_measures
#' @export
getPQRScore = function(times, cutoff = 3600, f = 10, p = 0.5, ...) {
  checkmate::assertNumeric(times, lower = 0, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(cutoff, lower = 1)
  checkmate::assertNumber(f, lower = 1)
  checkmate::assertNumber(p, lower = 0, upper = 1)

  m = length(times)
  threshold = floor(m * p + 1)
  threshold = min(m, threshold)
  n.success = sum(times < cutoff)
  if (n.success < threshold)
    return(f * cutoff)
  return(stats::quantile(times, p = p, names = FALSE, ...))
}

#' @name performance_measures
#' @export
getPERTScore = function(times, cutoff = 3600, f = 1, ...) {
  checkmate::assertNumeric(times, lower = 0, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(cutoff, lower = 1)
  checkmate::assertNumber(f, lower = 1)

  m = length(times)
  idx.in.time = which(times < cutoff)
  s = length(idx.in.time)
  a = b = 0
  if (s == 0)
    return(m * f * cutoff)
  ps = mean(times < cutoff)
  a = mean(times[times < cutoff])
  b = ((1 - ps) / ps) * f * cutoff
  return(a + b)
}

#' @name performance_measures
#' @export
getSuccessRate = function(times, cutoff = 3600) {
  mean(times < cutoff)
}

#' @name performance_measures
#' @export
getFailureRate = function(times, cutoff = 3600) {
  1 - getSuccessRate(times, cutoff)
}

#' @name performance_measures
#' @export
#FIXME: do we need this?
getRuntimeQuantile = function(times, cutoff = 3600, p = 0.5, ...) {
  checkmate::assertNumeric(times, lower = 0, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(cutoff, lower = 1)
  checkmate::assertNumber(p, lower = 0, upper = 1)

  times = times[times < cutoff]
  if (length(times) > 0L)
    return(stats::quantile(times, p = p, names = FALSE, ...))
  return(cutoff)
}

#' @name performance_measures
#' @export
#FIXME: generalize -> makeHVMeasure(...) where ... contains scalar goals
getHVScore = function(times, cutoff = 3600, p = 0.5, normalize = FALSE, ...) {
  checkmate::assertNumeric(times, lower = 0, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(cutoff, lower = 1)
  checkmate::assertNumber(p, lower = 0, upper = 1)
  checkmate::assertFlag(normalize)

  pf = getFailureRate(times, cutoff = cutoff, ...)
  rt = getRuntimeQuantile(times, cutoff = cutoff, ...)
  hv.score = (1 - pf) * (cutoff - rt)
  if (normalize)
    return(hv.score / cutoff)
  return(hv.score)
}
