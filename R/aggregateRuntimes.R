#' @title Runtime aggregation.
#'
#' @description Given a data frame of pure running times until a certain goal
#' has been reached, e.g., the optimum was found, computes different performance
#' scores by problem instance and algorithm/solver.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame with columns \dQuote{prob}, \dQuote{solver} and \dQuote{runtime}.
#' @param group.by [\code{character(1)}]\cr
#'   Which variables shall be used for grouping? Default is \code{c("prob", "solver")}.
#'   This parameter is useful if the naming of the columns deviates from the defaults.
#' @param runtime.col [\code{character(1)}]\cr
#'   Column name of column which contains the running times.
#'   Default is \code{runtime}.
#'   This parameter is useful if the naming of the columns deviates from the defaults.
#' @param scores [\code{list(list)}]\cr
#'   Named list of lists. The names on the first layer are passed as argument \code{score}
#'   to function \code{\link{getScore}}. Each element of the list is again an (eventually empty) named list
#'   with the paramters for the corresponding score, e.g., penalty factor \code{f} for PAR-score.
#' @param ... [any]
#'   Not used.
#' @return [data.frame] Data frame in long format. Basically \code{df} for each conbination of
#'   score and its parameters.
#' @examples
#' \dontrun{
#' % setup score parameters
#' f = c(1, 5, 10)
#' p = c(0.1, 0.5, 0.9)
#' cutoff = c(10, 25, 50)
#' aggr = aggregateRuntimes(df, scores = list(
#'   "PAR" = list(cutoff = cutoff, f = f),
#'   "PQR" = list(cutoff = cutoff, p = p, f = f))
#' )
#' }
#' @export
aggregateRuntimes = function(df, group.by = c("prob", "solver"), runtime.col = "runtime", scores, ...) {
  df.grouped = dplyr::group_by_(df, .dots = group.by)
  res = vector(mode = "list", length = length(scores))
  for (i in 1:length(scores)) {
    the.score = names(scores)[i]
    param.grid = do.call(base::expand.grid, scores[i])
    tmp = parallelMap::parallelMap(function(i) {
    #tmp = apply(param.grid, 1L, function(setting) {
      cat(".")
      setting = param.grid[i, ]
      tmp2 = dplyr::summarize(df.grouped, group = group[1L], "score" = as.numeric(do.call("getScore", c(list(times = runtime, score = the.score), as.list(setting)))))
      tmp2 = dplyr::ungroup(tmp2)
      for (param in names(setting))
        tmp2[[param]] = setting[[param]]
      tmp2[["score.value"]] = the.score
      return(tmp2)
    }, 1:nrow(param.grid))
    res[[i]] = do.call(plyr::rbind.fill, tmp)
  }
  return(do.call(plyr::rbind.fill, res))
}
