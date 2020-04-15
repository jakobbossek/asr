#' @title Performance scatter-plot
#'
#' @description Generates a scatter-plot of pairwise performances of two solvers.
#' The function expects a data frame \code{x} which contains performance values
#' row-wise. The mandatory column \dQuote{solver} encodes the name of the
#' corresponding algorithm.
#'
#' The procedure works as follows: the data frame \code{x} column is filtered
#' to all rows where \code{solver$x} is either \code{algo.a} or \code{algo.b}.
#' Next, a transformation of the data frame is conducted via \code{\link[reshape2]{dcast}}
#' to produce a wide data frame with each one column for \code{algo.a} and \code{algo.b}
#' respectively. These columns contain the performance values indicated by argument
#' \code{value.var}.
#'
#' @param x [\code{data.frame}]\cr
#'   Data frame with columns at least \dQuote{solver} and \code{value.var}.
#' @param value.var [\code{character(1)}]\cr
#'   Name of column which contains the performance values.
#' @param algo.a [\code{character(1)}]\cr
#'   Name of first algorithm (x-axis).
#' @param algo.b [\code{character(1)}]\cr
#'   Name of second algorithm (y-axis).
#' @param color [\code{character(1)}]\cr
#'   Name of column which determines coloring of points, e.g. instance group.
#' @param shape [\code{character(1)}]\cr
#'   Name of column which determines the shape of points, e.g. instance group.
#' @return [\code{ggplot}] ggplot2 object.
#' @export
plotScatterPerformance = function(x, value.var = "PAR10", algo.a, algo.b, color = "group", shape = "group") {
  checkmate::assertDataFrame(x)
  cns = colnames(x)
  if (!(value.var %in% cns)) {
    BBmisc::stopf("[asr::plotScatterPerformance] There is no column '%s' (value.var) in x.", value.var)
  }
  if (!("solver" %in% cns)) {
    BBmisc::stopf("[asr::plotScatterPerformance] Mandatory column 'solver' is missing in x.")
  }
  if (!(algo.a %in% x$solver) | !(algo.b %in% x$solver)) {
    BBmisc::stopf("[asr::plotScatterPerformance] Either '%s' (algo.a) or '%s' (algo.b) or both does/do not accur in x$solver.", algo.a, algo.b)
  }

  dd = x[x$solver %in% c(algo.a, algo.b), , drop = FALSE]

  # build formula: we need to convert to wide format (each one column for each solver)
  id.vars = setdiff(colnames(x), c(value.var, "solver"))
  formula = as.formula(sprintf("%s ~ solver", BBmisc::collapse(id.vars, sep = "+")))
  dd = reshape2::dcast(dd, formula = formula, value.var = value.var)

  pl = ggplot2::ggplot(dd)
  #FIXME: the majority of code here is for scales and hlines.
  # What if T is not equal to 3600s (1h)?
  pl = pl + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "solid", colour = "gray20", size = 1.3)
  pl = pl + ggplot2::geom_hline(yintercept = c(3600, 36000), linetype = "dashed", colour = "gray40")
  pl = pl + ggplot2::geom_vline(xintercept = c(3600, 36000), linetype = "dashed", colour = "gray40")
  pl = pl + ggplot2::geom_point(ggplot2::aes_string(x = algo.a, y = algo.b, colour = color, shape = shape), alpha = 0.7, size = 2)
  n.shapes = length(unique(dd[[shape]]))
  pl = pl + ggplot2::scale_x_log10(
    breaks = c(0, 1, 10, 100, 1000, 3600, 36000),
    labels = c("0", "1", "10", "100", "1000", "T = 3600", "10 x T"),
    limits = c(0.5, 36000))
  pl = pl + ggplot2::scale_y_log10(
    breaks = c(0, 1, 10, 100, 1000, 3600, 36000),
    labels = c("0", "1", "10", "100", "1000", "T = 3600", "10 x T"),
    limits = c(0.5, 36000))
  pl = pl + ggplot2::scale_shape_manual(values = c(1, 4, 5, 16, 6, 0, 17, 18)[seq_len(n.shapes)])
  pl = pl + ggplot2::theme_bw()
  pl = pl + ggplot2::scale_color_brewer(palette = "Dark2")
  pl = pl + ggplot2::labs(
    x = sprintf("Runtime (%s) of solver %s\n[in s; log-scaled] ", value.var, algo.a),
    y = sprintf("Runtime (%s) of solver %s\n[in s; log-scaled] ", value.var, algo.b),
    shape = tools::toTitleCase(shape),
    colour = tools::toTitleCase(color))
  pl = pl + ggplot2::theme(legend.position = "top", axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))
  pl = pl + ggplot2::guides(color = ggplot2::guide_legend(nrow = 1L), shape = ggplot2::guide_legend(nrow = 1L))
  return(pl)
}
