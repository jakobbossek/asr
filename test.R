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

parallelMap::parallelStartMulticore(4L)
f = c(1, 2, 5, 10, 100)
p = c(0, 0.25, 0.5, 0.75, 1.0)
cutoff = c(10, 25, 3600)
ee.aggr = aggregateRuntimes(ee, scores = list(
  "PAR" = list(cutoff = cutoff, f = f),
  "PQR" = list(cutoff = cutoff, p = p, f = f)))
print(ee.aggr)
parallelMap::parallelStop()

pl = ggplot(subset(ee.aggr, score.value == "PAR"), aes(x = group, y = score, fill = solver)) + geom_boxplot()
pl = pl + facet_grid(cutoff~f)
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
pl = pl + facet_grid(. ~ group + as.factor(cutoff))
pl = pl + viridis::scale_colour_viridis(discrete = TRUE, end = 0.8)
pl = pl + theme(legend.position = "bottom")
print(pl)

pl = ggplot(ee.unsolved, aes(x = as.factor(p), y = as.factor(cutoff), fill = log10(score))) + geom_tile()
pl = pl + viridis::scale_fill_viridis(discrete = FALSE, end = 0.8)
pl = pl + facet_grid(solver~group)
pl

stop("tadaaa!")
