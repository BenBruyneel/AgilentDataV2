test_that("getRtArea works",{
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
  expect_equal(
    getRtArea(theData = result[[1]], mzNr = 1), # within range select peak nr #
    data.frame(Component = c('KIKVYLPR_DON100_1'),
               Sequence = c('KIKVYLPR'),
               Location = c('277/279'),
               start = c(277),
               end = c(284),
               Modification = c('DON100'),
               mz = c('372.8974'),
               charge = c(3),
               rt = c(17.9),
               Peak = c(1),
               Reference = c(FALSE),
               Control = c(FALSE),
               polarity = c('+'),
               signal = c('ESI'),
               scan = c('EIC'),
               MS = c('Scan'),
               fragmentor = c(125),
               filename = c('WorklistData-0005.d'),
               smooth = c(FALSE),
               location = c('D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d'),
               Filename = c(2),
               foundStart = c(17.742),
               foundrt = c(17.897),
               foundEnd = c(17.989),
               foundHeight = c(7371),
               foundArea = c(36241),
               AreaPerc = c(28.32),
               HeightPerc = c(31.59),
               AreaSumPerc = c(12),
               Width = c(0.247),
               StartY = c(0),
               EndY = c(630),
               StartBLY = c(0),
               EndBLY = c(0),
               MaxY = c(7371),
               Baseline = c('BV'),
               BasePeak = c(791.3604),
               thePeakinTable = c(1))
  )
  expect_equal(
    getRtArea(theData = result[[1]], mzNr = 1,
              closeRts = list(left = 0.5, right = 0.5,
                              closest = TRUE, peak = TRUE)), # within range select closest peak
    data.frame(Component = c('KIKVYLPR_DON100_1'),
               Sequence = c('KIKVYLPR'),
               Location = c('277/279'),
               start = c(277),
               end = c(284),
               Modification = c('DON100'),
               mz = c('372.8974'),
               charge = c(3),
               rt = c(17.9),
               Peak = c(1),
               Reference = c(FALSE),
               Control = c(FALSE),
               polarity = c('+'),
               signal = c('ESI'),
               scan = c('EIC'),
               MS = c('Scan'),
               fragmentor = c(125),
               filename = c('WorklistData-0005.d'),
               smooth = c(FALSE),
               location = c('D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d'),
               Filename = c(2),
               foundStart = c(17.742),
               foundrt = c(17.897),
               foundEnd = c(17.989),
               foundHeight = c(7371),
               foundArea = c(36241),
               AreaPerc = c(28.32),
               HeightPerc = c(31.59),
               AreaSumPerc = c(12),
               Width = c(0.247),
               StartY = c(0),
               EndY = c(630),
               StartBLY = c(0),
               EndBLY = c(0),
               MaxY = c(7371),
               Baseline = c('BV'),
               BasePeak = c(791.3604),
               thePeakinTable = c(1))
  )
  expect_equal(
    getRtArea(theData = result[[1]], mzNr = 1,
              closeRts = list(left = 0.5, right = 0.5,
                              closest = FALSE, peak = FALSE)), # within range select highest peak
    data.frame(Component = c('KIKVYLPR_DON100_1'),
               Sequence = c('KIKVYLPR'),
               Location = c('277/279'),
               start = c(277),
               end = c(284),
               Modification = c('DON100'),
               mz = c('372.8974'),
               charge = c(3),
               rt = c(17.9),
               Peak = c(1),
               Reference = c(FALSE),
               Control = c(FALSE),
               polarity = c('+'),
               signal = c('ESI'),
               scan = c('EIC'),
               MS = c('Scan'),
               fragmentor = c(125),
               filename = c('WorklistData-0005.d'),
               smooth = c(FALSE),
               location = c('D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d'),
               Filename = c(3),
               foundStart = c(18.022),
               foundrt = c(18.121),
               foundEnd = c(18.253),
               foundHeight = c(23333),
               foundArea = c(127951),
               AreaPerc = c(100),
               HeightPerc = c(100),
               AreaSumPerc = c(42.37),
               Width = c(0.231),
               StartY = c(958),
               EndY = c(122),
               StartBLY = c(122),
               EndBLY = c(122),
               MaxY = c(23455),
               Baseline = c('VB'),
               BasePeak = c(372.8978),
               thePeakinTable = c(2))
  )
  # cases where one item needed did not load
  result2 <- result
  result2[[1]]$targets <- NA
  expect_equal(
    getRtArea(theData = result2[[1]], mzNr = 1),
    NA
  )
  result2 <- result
  result2[[1]]$peakList <- NA
  expect_equal(
    getRtArea(theData = result2[[1]], mzNr = 1),
    NA
  )
  # cases where a component is missing
  result2 <- result
  result2[[1]]$peakList <- result[[1]]$peakList %>%
    dplyr::filter(formatDigitsLargeNumbers(4)(mz) != "750.3818")
  expect_equal(
    nrow(getRtArea(theData = result2[[1]], mzNr = 20,
                   closeRts = list(left = 0.5, right = 0.5,
                                   closest = FALSE, peak = TRUE),
                   includeMissing = FALSE)),
    0
  )
  expect_equal(
    nrow(getRtArea(theData = result2[[1]], mzNr = 20,
                   closeRts = list(left = 0.5, right = 0.5,
                                   closest = FALSE, peak = TRUE))),
    1
  )
  test <- getRtArea(theData = result[[1]], mzNr = NA,
                    closeRts = list(left = 0.5, right = 0.5,
                                    closest = FALSE, peak = TRUE))
  test2 <- getRtArea(theData = result2[[1]], mzNr = NA,
                     closeRts = list(left = 0.5, right = 0.5,
                                     closest = FALSE, peak = TRUE), includeMissing = TRUE)
  expect_equal(lapply(test, class), lapply(test, class))
  test2 <- getRtArea(theData = result2[[1]], mzNr = NA,
                     closeRts = list(left = 0.5, right = 0.5,
                                     closest = FALSE, peak = TRUE), includeMissing = FALSE)
  expect_equal(lapply(test, class), lapply(test, class))
})

test_that("getRRtReference works",{
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
  expect_equal(
    formatDigitsLargeNumbers(2)(getRRtReference(theData = result[[1]])),
    "28.69"
  )
  expect_equal(
    formatDigitsLargeNumbers(2)(getRRtReference(theData = result[[1]],
                                                fixedRrt = 10)),
    "10.00"
  )
})

test_that("getRtArea works",{
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
  expect_equal(
    getRRtArea(theData = result[[1]], mzNr = 1), # within range select peak nr #
    data.frame(Component = c('KIKVYLPR_DON100_1'),
               Sequence = c('KIKVYLPR'),
               Location = c('277/279'),
               start = c(277),
               end = c(284),
               Modification = c('DON100'),
               mz = c('372.8974'),
               charge = c(3),
               rt = c(17.9),
               Peak = c(1),
               Reference = c(FALSE),
               Control = c(FALSE),
               polarity = c('+'),
               signal = c('ESI'),
               scan = c('EIC'),
               MS = c('Scan'),
               fragmentor = c(125),
               filename = c('WorklistData-0005.d'),
               smooth = c(FALSE),
               location = c('D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d'),
               Filename = c(2),
               foundStart = c(17.742),
               foundrt = c(17.897),
               foundEnd = c(17.989),
               foundHeight = c(7371),
               foundArea = c(36241),
               AreaPerc = c(28.32),
               HeightPerc = c(31.59),
               AreaSumPerc = c(12),
               Width = c(0.247),
               StartY = c(0),
               EndY = c(630),
               StartBLY = c(0),
               EndBLY = c(0),
               MaxY = c(7371),
               Baseline = c('BV'),
               BasePeak = c(791.3604),
               thePeakinTable = c(2))
  )
  # cases where one item needed did not load
  result2 <- result
  result2[[1]]$targets <- NA
  expect_equal(
    getRRtArea(theData = result2[[1]], mzNr = 1),
    NA
  )
  result2 <- result
  result2[[1]]$peakList <- NA
  expect_equal(
    getRRtArea(theData = result2[[1]], mzNr = 1),
    NA
  )
  # cases where a component is missing
  result2 <- result
  result2[[1]]$peakList <- result[[1]]$peakList %>%
    dplyr::filter(formatDigitsLargeNumbers(4)(mz) != "750.3818")
  expect_equal(
    nrow(getRRtArea(theData = result2[[1]], mzNr = 20,
                   closeRts = list(left = 0.5, right = 0.5,
                                   closest = FALSE, peak = TRUE),
                   includeMissing = FALSE)),
    0
  )
  expect_equal(
    nrow(getRRtArea(theData = result2[[1]], mzNr = 20,
                   closeRts = list(left = 0.5, right = 0.5,
                                   closest = FALSE, peak = TRUE))),
    1
  )
  test <- getRRtArea(theData = result[[1]], mzNr = NA,
                    closeRts = list(left = 0.5, right = 0.5,
                                    closest = FALSE, peak = TRUE))
  test2 <- getRRtArea(theData = result2[[1]], mzNr = NA,
                      closeRts = list(left = 0.5, right = 0.5,
                                      closest = FALSE, peak = TRUE), includeMissing = TRUE)
  expect_equal(lapply(test, class), lapply(test, class))
  test2 <- getRRtArea(theData = result2[[1]], mzNr = NA,
                      closeRts = list(left = 0.5, right = 0.5,
                                      closest = FALSE, peak = TRUE), includeMissing = FALSE)
  expect_equal(lapply(test, class), lapply(test, class))
})

test_that("getMzs works",{
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
  expect_equal(getMzs(theData = result[[1]],
                      thePeaks = thePeaks,
                      mzNr = 1),
               data.frame(Component = c('KIKVYLPR_DON100_1'),
                          Sequence = c('KIKVYLPR'),
                          Location = c('277/279'),
                          start = c(277),
                          end = c(284),
                          Modification = c('DON100'),
                          mz = c('372.8974'),
                          charge = c(3),
                          rt = c(17.9),
                          Peak = c(1),
                          Reference = c(FALSE),
                          Control = c(FALSE),
                          polarity = c('+'),
                          signal = c('ESI'),
                          scan = c('EIC'),
                          MS = c('Scan'),
                          fragmentor = c(125),
                          filename = c('WorklistData-0005.d'),
                          smooth = c(FALSE),
                          location = c('D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d'),
                          Filename = c(2),
                          foundStart = c(17.742),
                          foundrt = c(17.897),
                          foundEnd = c(17.989),
                          foundHeight = c(7371),
                          foundArea = c(36241),
                          AreaPerc = c(28.32),
                          HeightPerc = c(31.59),
                          AreaSumPerc = c(12),
                          Width = c(0.247),
                          StartY = c(0),
                          EndY = c(630),
                          StartBLY = c(0),
                          EndBLY = c(0),
                          MaxY = c(7371),
                          Baseline = c('BV'),
                          BasePeak = c(791.3604),
                          thePeakinTable = c(2),
                          foundmz = c(372.8975)))
  result <- getMzs(theData = result[[1]],
                   thePeaks = thePeaks,
                   mzNr = NA)
  expect_equal(dim(result), c(36, 39))
  expect_equal(colnames(result),
               c('Component','Sequence','Location','start','end','Modification',
                 'mz','charge','rt','Peak','Reference','Control','polarity',
                 'signal','scan','MS','fragmentor','filename','smooth',
                 'location','Filename','foundStart','foundrt','foundEnd',
                 'foundHeight','foundArea','AreaPerc','HeightPerc','AreaSumPerc',
                 'Width','StartY','EndY','StartBLY','EndBLY','MaxY','Baseline',
                 'BasePeak','thePeakinTable','foundmz'))
  expect_equal(sum(is.na(result$foundmz)), 0)
  expect_equal(sum(is.na(result$foundrt)), 0)
  expect_equal(sum(is.na(result$foundArea)), 0)
})
