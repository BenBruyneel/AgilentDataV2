test_that("mzData works", {
  demoFile <- fs::path_package(
    "extdata",
    "raw/WKL-0001.mzdata.xml",
    package = "AgilentDataV2"
  )
  demoFile
  spectrData <- mzData$new(filename = demoFile)
  expect_equal(class(spectrData), c("mzData", "R6"))
  expect_equal(spectrData$length, 2)
  expect_equal(dim(spectrData$spectrum(number = 1)), c(25953, 2))
  expect_equal(dim(spectrData$spectrum(number = 2)), c(24947, 2))
  expect_equal(
    formatC(sum(spectrData$spectrum(number = 1)$mz, na.rm = TRUE), digits = 8),
    " 22070885"
  )
  expect_equal(
    formatC(sum(spectrData$spectrum(number = 2)$mz, na.rm = TRUE), digits = 8),
    " 22134568"
  )
  expect_equal(
    formatC(
      sum(spectrData$spectrum(number = 1)$intensity, na.rm = TRUE),
      digits = 8
    ),
    "  3006477"
  )
  expect_equal(
    formatC(
      sum(spectrData$spectrum(number = 2)$intensity, na.rm = TRUE),
      digits = 8
    ),
    "6564663.6"
  )
  demoFile <- fs::path_package(
    "extdata",
    "raw/WorklistData-0005.mzdata.xml",
    package = "AgilentDataV2"
  )
  demoFile
  spectrData <- mzData$new(filename = demoFile)
  expect_equal(spectrData$length, 148)
  expect_equal(class(spectrData), c("mzData", "R6"))
  expect_equal(
    spectrData$info(centroided = TRUE) |> head(5),
    data.frame(spectrumType = 'continuous',
               methodOfCombination = 'average',
               count = '1',
               ScanMode = 'Scan',
               Polarity = 'Positive',
               TimeInMinutes = c('8.835-8.947','14.406-14.538','55.328-55.414','16.900-17.029','17.808-17.983'),
               msLevel = '1',
               mzRangeStart = c('200.002175541493','200.001290197798','200.001203767719','200.001957602523','200.005267948082'),
               mzRangeStop = c('1998.36530048828','1998.7943710028','1997.65898733945','1998.64996647852','1998.81042231457'),
               Start = c( 8.835,14.406,55.328,16.900,17.808),
               End = c( 8.947,14.538,55.414,17.029,17.983),
               centroided = TRUE)
  )
})

test_that("getData.Spectra works",{
  demoFile <- fs::path_package(
    "extdata",
    "raw/WKL-0001.mzdata.xml",
    package = "AgilentDataV2"
  )
  demoFile
  spectrData <- getData.Spectra(filename = demoFile)
  expect_equal(class(spectrData), c("mzData", "R6"))
  expect_equal(spectrData$length, 2)
  expect_equal(dim(spectrData$spectrum(number = 1)), c(25953, 2))
  expect_equal(dim(spectrData$spectrum(number = 2)), c(24947, 2))
  expect_equal(
    formatC(sum(spectrData$spectrum(number = 1)$mz, na.rm = TRUE), digits = 8),
    " 22070885"
  )
  expect_equal(
    formatC(sum(spectrData$spectrum(number = 2)$mz, na.rm = TRUE), digits = 8),
    " 22134568"
  )
  expect_equal(
    formatC(
      sum(spectrData$spectrum(number = 1)$intensity, na.rm = TRUE),
      digits = 8
    ),
    "  3006477"
  )
  expect_equal(
    formatC(
      sum(spectrData$spectrum(number = 2)$intensity, na.rm = TRUE),
      digits = 8
    ),
    "6564663.6"
  )
  demoFile <- fs::path_package(
    "extdata",
    "raw/WorklistData-0005.mzdata.xml",
    package = "AgilentDataV2"
  )
  demoFile
  spectrData <- getData.Spectra(filename = demoFile)
  expect_equal(spectrData$length, 148)
  expect_equal(class(spectrData), c("mzData", "R6"))
  expect_equal(
    spectrData$info(centroided = TRUE) |> head(5),
    data.frame(spectrumType = 'continuous',
               methodOfCombination = 'average',
               count = '1',
               ScanMode = 'Scan',
               Polarity = 'Positive',
               TimeInMinutes = c('8.835-8.947','14.406-14.538','55.328-55.414','16.900-17.029','17.808-17.983'),
               msLevel = '1',
               mzRangeStart = c('200.002175541493','200.001290197798','200.001203767719','200.001957602523','200.005267948082'),
               mzRangeStop = c('1998.36530048828','1998.7943710028','1997.65898733945','1998.64996647852','1998.81042231457'),
               Start = c( 8.835,14.406,55.328,16.900,17.808),
               End = c( 8.947,14.538,55.414,17.029,17.983),
               centroided = TRUE)
  )
})
