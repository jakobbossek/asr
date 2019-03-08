# Pairwise scatterplots of performance values
plotScatter = function(df, value.var = "PAR10", algo.a, algo.b, gg.colour = "group", gg.shape = "group") {
  dd = df[df$solver %in% c(algo.a, algo.b), , drop = FALSE]
  dd = reshape2::dcast(dd, group + size + prob ~ solver, value.var = value.var)

  pl = ggplot2::ggplot(dd)
  pl = pl + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "solid", colour = "gray20", size = 1.3)
  pl = pl + ggplot2::geom_hline(yintercept = c(3600, 36000), linetype = "dashed", colour = "gray40")
  pl = pl + ggplot2::geom_vline(xintercept = c(3600, 36000), linetype = "dashed", colour = "gray40")
  pl = pl + ggplot2::geom_point(ggplot2::aes_string(x = algo.a, y = algo.b, colour = gg.colour, shape = gg.shape), size = 2)
  n.shapes = length(unique(dd[[gg.shape]]))
  pl = pl + ggplot2::scale_x_log10(
    breaks = c(0, 1, 10, 100, 1000, 3600, 36000),
    labels = c("0", "1", "10", "100", "1000", "T = 3600", "10 x T"),
    limits = c(0.5, 36000))
  pl = pl + ggplot2::scale_y_log10(
    breaks = c(0, 1, 10, 100, 1000, 3600, 36000),
    labels = c("0", "1", "10", "100", "1000", "T = 3600", "10 x T"),
    limits = c(0.5, 36000))
  pl = pl + ggplot2::scale_shape_manual(values = c(0, 1, 5, 6)[seq_len(n.shapes)])
  pl = pl + theme_bw()
  pl = pl + viridis::scale_colour_viridis(discrete = TRUE, end = 0.75)
  #pl = pl + ggplot2::scale_colour_grey(end = 0.7)
  pl = pl + labs(
    x = sprintf("Runtime (%s) of solver %s\n[in s; log-scaled] ", value.var, algo.a),
    y = sprintf("Runtime (%s) of solver %s\n[in s; log-scaled] ", value.var, algo.b),
    shape = tools::toTitleCase(gg.shape),
    colour = tools::toTitleCase(gg.colour))
  pl = pl + ggplot2::theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45))
  pl = pl + ggplot2::guides(color = guide_legend(nrow = 2L), shape = guide_legend(nrow = 2L))
  #pl = pl + ggplot2::facet_grid(. ~ size)
#  print(pl)
  return(pl)
}

# Pairwise scatterplots of feature values
plotFeatScatter = function(df,
  x, y,
  gg.colour = NULL, gg.shape = NULL,
  log.colour = FALSE,
  facet.args = NULL,
  ...) {
  cns = colnames(df)
  if (is.numeric(x))
    x = cns[x]
  if (is.numeric(y))
    y = cns[y]
  n.shapes = length(unique(df[[gg.shape]]))
  discrete.colour = (is.factor(df[[gg.colour]]) | is.character(df[[gg.colour]]))
  if (log.colour)
    df[[gg.colour]] = log(df[[gg.colour]] + 1)

  n = length(x)
  if (n != length(y))
    BBmisc::stopf("[plotFeatScatter] x and y must have the same length.")

  pls = lapply(seq_len(n), function(i) {
    pl = ggplot(df)
    pl = pl + geom_point(aes_string(x = x[i], y = y[i], colour = gg.colour, shape = gg.shape))
    pl = pl + labs(
      x = x[i],
      y = y[i],
      shape = tools::toTitleCase(gg.shape),
      colour = tools::toTitleCase(gg.colour))
    pl = pl + theme_bw()
    pl = pl + theme(legend.position = "top")
    pl = pl + ggplot2::scale_shape_manual(values = c(0, 1, 5, 6)[seq_len(n.shapes)])
    pl = pl + viridis::scale_colour_viridis(discrete = discrete.colour, end = 0.75)
    if (!is.null(facet.args))
      pl = pl + do.call(ggplot2::facet_wrap, facet.args)
    return(pl)
  })

  if (length(pls) == 1L)
    return(pls[[1L]])

  args = c(pls, list(common.legend = TRUE, legend = "top", ncol = n))
  args = BBmisc::insert(args, list(...))
  return(do.call(ggpubr::ggarrange, args))
}
