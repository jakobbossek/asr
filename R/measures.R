estimateSuccessRate = function(times, cutoff = 3600) {
  mean(times < cutoff)
}

estimateFailureRate = function(times, cutoff = 3600) {
  1 - estimateSuccessRate(times, cutoff)
}

estimateRuntimeQuantile = function(times, cutoff = 3600, p = 0.5, ...) {
  times = times[times < cutoff]
  if (length(times) > 0L)
    return(quantile(times, p = p, names = FALSE, ...))
  return(cutoff)
}

getPARScore = function(times, cutoff = 3600, f = 10, ...) {
  idx.timed.out = which(times >= cutoff)
  if (length(idx.timed.out) > 0L)
    times[idx.timed.out] = f * cutoff
  return(mean(times))
}

getPQRScore = function(times, cutoff = 3600, f = 10, p = 0.5, ...) {
  m = length(times)
  threshold = floor(m * p + 1)
  threshold = min(m, threshold)
  n.success = sum(times < cutoff)
  if (n.success < threshold)
    return(f * cutoff)
  return(quantile(times, p = p, names = FALSE, ...))
}

getPERTScore = function(times, cutoff = 3600, f = 10, ...) {
  m = length(times)
  idx.in.time = which(times < cutoff)
  s = length(idx.in.time)
  a = b = 0
  if (s == 0)
    return(m * f * cutoff)
  ps = s / m
  a = mean(times[idx.in.time])
  b = ((1 - ps) / ps) * f * T
  return(a + b)
}
