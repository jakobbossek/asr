library(methods)
library(devtools)
library(testthat)
library(smoof)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(viridis)
library(microbenchmark)
library(rlang)

load_all(".")

# read wide format
dd = readr::read_csv("data/tsp_runtimes.csv")

# convert to long format
ee = reshape2::melt(dd, id.vars = c("prob", "group", "it", "size"), value.name = "runtime", variable.name = "solver")

aggregateRuntimes = function(df, group.by = c("prob", "solver"), runtime.col = "runtime", scores, ...) {
  df.grouped = dplyr::group_by_(df, .dots = group.by)
  res = vector(mode = "list", length = length(scores))
  for (i in 1:length(scores)) {
    the.score = names(scores)[i]
    param.grid = do.call(base::expand.grid, scores[i])
    tmp = apply(param.grid, 1L, function(setting) {
      tmp2 = dplyr::summarize(df.grouped, group = group[1L], "score" = as.numeric(do.call("getScore", c(list(times = runtime, score = the.score), as.list(setting)))))
      tmp2 = dplyr::ungroup(tmp2)
      for (param in names(setting))
        tmp2[[param]] = setting[[param]]
      tmp2[["score.value"]] = the.score
      return(tmp2)
    })
    res[[i]] = do.call(plyr::rbind.fill, tmp)
  }
  return(do.call(plyr::rbind.fill, res))
}

ee.aggr = aggregateRuntimes(ee, scores = list(
  #"PAR" = list(cutoff = c(10, 25, 50, 100, 200, 500, 1000, 3600) , f = 1:10),
  "PQR" = list(cutoff = c(10, 25, 50, 100), p = seq(0.1, 1.0, by = 0.1))))
print(ee.aggr)

pl = ggplot(subset(ee.aggr, score.value == "PAR"), aes(x = group, y = score, fill = solver)) + geom_boxplot()
pl = pl + facet_grid(cutoff~.)
pl = pl + viridis::scale_fill_viridis(discrete = TRUE, end = 0.8)
print(pl + scale_y_log10())

pl = ggplot(subset(ee.aggr, score.value == "PAR"), aes(x = as.factor(f), y = as.factor(cutoff), fill = log10(score))) + geom_tile()
pl = pl + viridis::scale_fill_viridis(discrete = FALSE, end = 0.8)
pl = pl + facet_grid(solver~group)
pl

ee.unsolved = subset(ee.aggr, score.value == "PQR") %>%
  group_by(group, solver, p, cutoff) %>%
  summarize(unsolved = mean(score > cutoff)) %>%
  ungroup()

pl = ggplot(ee.unsolved, aes(x = p, y = unsolved, colour = solver, linetype = solver)) + geom_line() + geom_point()
pl = pl + facet_grid(as.factor(cutoff) ~ group)
pl = pl + viridis::scale_colour_viridis(discrete = TRUE, end = 0.8)
pl = pl + theme(legend.position = "bottom")
print(pl)

stop("tadaaa!")


ee.aggr = ee %>%
  group_by(prob, solver) %>%
  dplyr::summarize(par10 = getScore(runtime, score = "PAR", cutoff = 3600)) %>%
  slice(1) %>%
  ungroup()


pl = ggplot(ee, aes(x = size, y = runtime)) + geom_point()# + facet_grid(group ~ solver)
print(pl)

pl = ggplot(ee, aes(x = runtime, y = ..density..)) + geom_density(aes(fill = group), alpha = .3)# + facet_grid(group ~ solver)
pl + scale_x_log10()
