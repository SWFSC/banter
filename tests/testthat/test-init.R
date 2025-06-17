context("init")

data(train.data)
mdl <- initBanterModel(train.data$events)
num.train.events <- as.vector(table(train.data$events$species))
num.mdl.events <- numEvents(mdl)$num.events
spp.train.events <- sort(unique(train.data$events$species))
spp.mdl.events <- sort(numEvents(mdl)$species)

test_that("initBanterModel loads all events from data.", {
  expect_equal(num.train.events, num.mdl.events)
  expect_equal(spp.train.events, spp.mdl.events)
})