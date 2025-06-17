rm(list = ls())
library(tidyverse)
library(banter)
library(rfPermute)

## test package example data

data(train.data)
ex.mdl <- initBanterModel(train.data$events) |>
  addBanterDetector(train.data$detectors, ntree = 100, sampsize = 1) |>
  runBanterModel(ntree = 100, sampsize = 1)
summary(ex.mdl)

data(test.data)
predict(ex.mdl, test.data)



## test CalCurCEAS data

load("data/calcurceas data.rdata")

ntree <- 1000
sampsize <- 5

# Initialize model
mdl <- initBanterModel(survey.train$events)
# Add detectors using list
mdl <- addBanterDetector(
  mdl, 
  survey.train$detectors[c("bp", "dw")], 
  ntree = ntree,
  sampsize = sampsize,
  num.cores = 1
)
# Add detectors using data.frame
mdl <- addBanterDetector(
  mdl, 
  data = survey.train$detectors$ec, 
  name = "ec", 
  ntree = ntree, 
  sampsize = sampsize
)
# Run model
mdl <- runBanterModel(mdl, 5000, 2)

# Model Summaries
summary(mdl, bins = 20)
rf <- getBanterModel(mdl)
proximityPlot(rf)
plotVotes(rf)
impHeatmap(rf, 20)
plotImpVarDist(rf, getBanterModelData(mdl), "species")
plotPredictedProbs(rf)

# Prediction
test.pred <- predict(mdl, survey.test)
for(x in test.pred) print(head(x))

#save.image("data/test ws.rdata")
