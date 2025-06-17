rm(list = ls())
library(tidyverse)
library(rfPermute)
library(banter)
library(gridExtra)

load("data/calcurceas data.rdata")

ccc.mdl <- initBanterModel(survey.train$events) |> 
  addBanterDetector(survey.train$detectors, ntree = 10000, sampsize = 100) |> 
  runBanterModel(ntree = 10000, sampsize = 3)

pdf("CalCurCEAS plots.pdf")

summary(ccc.mdl, bins = 20)

rf <- getBanterModel(ccc.mdl)
impHeatmap(rf, 20)
plotImpVarDist(rf, getBanterModelData(ccc.mdl), "species")
plotPredictedProbs(rf)
plotVotes(rf)
proximityPlot(rf)

dev.off()

save(ccc.mdl, file = "data/ccc banter.rdata")
