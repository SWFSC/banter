context("model accessors")

data(train.data)
mdl <- initBanterModel(train.data$events) |> 
  addBanterDetector(
    train.data$detectors, 
    ntree = 50, sampsize = 1, num.cores = 1
  ) 

test_that("getDetectorNames returns correct names", {
  expect_equal(getDetectorNames(mdl), names(train.data$detectors))
})

test_that("numCalls returns number of calls in each detector", {
  expect_known_value(numCalls(mdl), "test-numCalls")
})

test_that("getSampSize returns correct sample sizes", {
  mdl <- expect_warning(runBanterModel(mdl, ntree = 50, sampsize = 1))
  
  expect_true(all(getSampSize(mdl) == 1))
  expect_true(all(getSampSize(mdl, "bp") == 1))
  expect_true(all(getSampSize(mdl, "dw") == 1))
  expect_true(all(getSampSize(mdl, "ec") == 1))
})

