test_that("giveLocation works",{
  expect_equal(
    giveLocation(),
    "55"
  )
  expect_equal(
    giveLocation("DILNQITKPNDVYSFSLASR"),
    "92"
  )
  expect_equal(
    giveLocation("LYAEERYPILPEYLQCVKELYR"),
    "122"
  )
  expect_equal(
    giveLocation("VTEQESKPVQMMYQIGLFR"),
    "206"
  )
  expect_equal(
    giveLocation("VASMASEKMK"),
    "226"
  )
  expect_equal(
    giveLocation("KIKVYLPR"),
    "277/279"
  )
  expect_equal(
    giveLocation("KIKVYLP"),
    as.character(NA)
  )
})

test_that("giveLabel works",{
  expect_equal(
    giveLabel("DON"),
    "DON"
  )
  expect_equal(
    giveLabel("DON, DON"),
    "DON"
  )
  expect_equal(
    giveLabel("DON, DON82"),
    "DON"
  )
  expect_equal(
    giveLabel("DON82"),
    ""
  )
})

test_that("giveImpurity works",{
  expect_equal(
    giveImpurity("DON100"),
    "B"
  )
  expect_equal(
    giveImpurity("DON82"),
    "A"
  )
  expect_equal(
    giveImpurity("DON"),
    ""
  )
  expect_equal(
    giveImpurity("Nothing"),
    ""
  )
})

test_that("completeTable2Signals works",{
  demoSpecFile <- fs::path_package(
    "extdata",
    "raw/WorklistData-0005.mzdata.xml",
    package = "AgilentDataV2"
  )
  demoPeaksFile <- fs::path_package(
    "extdata",
    "raw/WorklistData-0005_MS.csv",
    package = "AgilentDataV2"
  )
  demoTargetsFile <- fs::path_package(
    "extdata",
    "targets/targets001.csv",
    package = "AgilentDataV2"
  )
  result <- getData.All(
    sampleName = "WKL-0005",
    peptideTargetsFile = demoTargetsFile,
    mzDataFile = demoSpecFile,
    msPeakFile = demoPeaksFile,
  )
  thePeaks <- getRRtArea(theData = result[[1]])
  getMzs(theData = result[[1]],
         thePeaks = thePeaks,
         mzNr = 1)
  result <- getMzs(theData = result[[1]],
                   thePeaks = thePeaks,
                   mzNr = NA)
  result <- completeTable2Signals(result)
  expect_equal(
    dim(result),
    c(36,13)
  )
  result <- completeTable2Signals(NA)
  expect_equal(
    dim(result),
    c(0,13)
  )
})
