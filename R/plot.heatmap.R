plotPerformanceHeatmap = function(x,
  perf1, perf2,
  perf1.splits,
  perf2.splits) {

  checkmate::assertDataFrame(x, min.rows = 1L, min.cols = 3L)
  checkmate::assertString(perf1)
  checkmate::assertString(perf2)

  if (!checkmate::checkSubset(c(perf1, perf2), names(x))) {
    BBmisc::stopf("One column per performanc measure required, but there there are no columns named '%s'",
      BBmisc::collapse(setdiff(names(x), c(perf1, perf2))), sep = ", ")
  }

  if (!("group" %in% colnames(x)))
    x$group = "ALL"

  #FIXME: very ugly!
  newdat = data.frame()
  for (algo in unique(x$solver)) {
    for (group in unique(x$group)) {
      tmp = x[x$solver == algo & x$group == group, , drop = FALSE]
      for (i in 1:length(perf1.splits)) {
        for (j in 2:length(perf2.splits)) {
          tmp2 = tmp[(tmp[[perf1]] == perf1.splits[i]) & (tmp[[perf2]] > perf2.splits[j-1]) & (tmp[[perf2]] <= perf2.splits[j]), , drop = FALSE]
          #tmp2 = tmp[(tmp[[perf1]] >= (perf1.splits[i] - 0.01)) & (tmp[[perf1]] <= (perf1.splits[i] + 0.01)) & (tmp[[perf2]] >= perf2.splits[j-1]) & (tmp[[perf2]] < perf2.splits[j]), , drop = FALSE]
          newdat2 = data.frame(
            algo = algo,
            group = group,
            count = nrow(tmp2),
            perf1 = sprintf("%.1f", perf1.splits[i]),
            perf2 = sprintf("%.0f-%.0f", perf2.splits[j-1], perf2.splits[j]))
          newdat = rbind(newdat, newdat2)
        }
      }
    }
  }

  pl = ggplot2::ggplot(newdat, ggplot2::aes_string(x = "perf2", y = "perf1"))
  pl = pl + ggplot2::geom_tile(ggplot2::aes_string(fill = "log(count + 1)"), color = "white", size = 0.1)
  pl = pl + ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))
  pl = pl + ggplot2::facet_grid(group ~ algo)
  pl = pl + viridis::scale_fill_viridis(end = 0.9)#, guide = guide_legend(direction = "horizontal"))
  newdat.text = newdat
  newdat.text[newdat.text$count == 0, "count"] = NA
  pl = pl + ggplot2::geom_text(data = newdat.text, ggplot2::aes(label = count), color = "white", size = 1.9)
  pl = pl + ggplot2::coord_fixed()
  #pl = pl + guide_()
  pl = pl + ggplot2::labs(
    x = "Performance 2",
    y = "Performance 1",
    fill = "#of solved instances\n(log-scale)"
  )
  return(pl)
}

