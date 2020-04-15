#Bi-obj. (RTS, PF)->min! measure
#FIXME: needs generalization
plotBiobjectiveScatter = function(df, x.value.var, y.value.var, xlim = NULL, ylim = NULL, gg.colour = "group", gg.shape = "group") {
  pl = ggplot2::ggplot(df)
  pl = pl + geom_point(aes_string(x = x.value.var, y = y.value.var, color = gg.colour, shape = gg.shape))
  n.shapes = length(unique(df[[gg.shape]]))
  pl = pl + ggplot2::scale_shape_manual(values = c(0, 1, 5, 6, 16, 17, 18)[seq_len(n.shapes)])
  pl = pl + ggplot2::scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = as.character(seq(0, 1, by = 0.2)))
#    limits = c(-0.1, 1.2))
  pl = pl + ggplot2::scale_x_log10(
    breaks = c(0, 1, 10, 100, 1000, 3600),
    labels = c("0", "1", "10", "100", "1000", "T = 3600"),
    limits = c(0.5, 3650))
  pl = pl + theme_bw()
  pl = pl + viridis::scale_colour_viridis(discrete = TRUE, end = 0.75)
  pl = pl + ggplot2::theme(legend.position = "top")
  pl = pl + ggplot2::guides(color = guide_legend(nrow = 2L), shape = guide_legend(nrow = 2L))
  pl = pl + labs(
    x = "Running time of successful runs [in s; log-scaled]",
    y = "Fraction of failed runs",
    shape = tools::toTitleCase(gg.shape),
    colour = tools::toTitleCase(gg.colour))
  pl = pl + facet_grid(. ~ solver)
  pl
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
    pl = pl + ggplot2::scale_shape_manual(values = c(0, 1, 5, 6, 16, 17, 18)[seq_len(n.shapes)])
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
