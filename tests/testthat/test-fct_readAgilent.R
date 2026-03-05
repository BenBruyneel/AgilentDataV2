test_that("chromatogramInfo.AgilentExport works",{
  result <- chromatogramInfo.AgilentExport("+ESI TIC Scan Frag=125.0V Data0001.d ")
  expect_equal(
    result,
    data.frame(polarity = c('+'),
               signal = c('ESI'),
               scan = c('TIC'),
               mz = as.numeric(NA),
               MS = c('Scan'),
               fragmentor = c(125),
               filename = c('Data0001.d'),
               smooth = c(FALSE))
  )
  result <- chromatogramInfo.AgilentExport(" +ESI EIC(319.8662) Scan Frag=125.0V WorklistData-0005.d ")
  expect_equal(
    result,
    data.frame(polarity = c('+'),
               signal = c('ESI'),
               scan = c('EIC'),
               mz = c(319.8662),
               MS = c('Scan'),
               fragmentor = c(125),
               filename = c('WorklistData-0005.d'),
               smooth = c(FALSE))
  )
  result <- chromatogramInfo.AgilentExport(" +ESI EIC(319.8662) Scan Frag=125.0V WorklistData-0005.d ")
  expect_equal(
    result,
    data.frame(polarity = c('+'),
               signal = c('ESI'),
               scan = c('EIC'),
               mz = c(319.8662),
               MS = c('Scan'),
               fragmentor = c(125),
               filename = c('WorklistData-0005.d'),
               smooth = c(FALSE))
  )
})

test_that("peakListInfo.AgilentExport works",{
  result <- peakListInfo.AgilentExport("D:/MassHunter/Data/Ben/Data0001.d, +ESI TIC Scan Frag=125.0V Data0001.d ")
  expect_equal(
    result,
    data.frame(polarity = c('+'),
               signal = c('ESI'),
               scan = c('TIC'),
               mz = as.numeric(NA),
               MS = c('Scan'),
               fragmentor = c(125),
               filename = c('Data0001.d'),
               smooth = c(FALSE),
               location = c('D:/MassHunter/Data/Ben/Data0001.d'))
  )
  result <- peakListInfo.AgilentExport("D:/MassHunter/Data/Ben/Data0001.d, +ESI EIC(591.2807) Scan Frag=125.0V Data0001.d ")
  expect_equal(
    result,
    data.frame(polarity = c('+'),
               signal = c('ESI'),
               scan = c('EIC'),
               mz = c(591.2807),
               MS = c('Scan'),
               fragmentor = c(125),
               filename = c('Data0001.d'),
               smooth = c(FALSE),
               location = c('D:/MassHunter/Data/Ben/Data0001.d'))
  )
})

test_that("readPeaklist.AgilentExport.memory works", {
  demoFile <- fs::path_package("extdata", "raw/WorklistData-0005_MS.csv", package = "AgilentDataV2")
  result <- readLines(demoFile)
  result <- readPeaklist.AgilentExport.memory(result, startsString = "WorklistData-0005")()
  expect_length(result, 2)
  expect_equal(names(result), c("info", "data"))
  expect_equal(nrow(result$info), 35)
  expect_equal(nrow(result$info), length(result$data))
  expect_equal(result$data[[1]], NA)
  expect_equal(
    result$data[[4]],
    data.frame(Filename = c('D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d '),
               Peak = c(1),
               Start = c(8.766),
               RT = c(8.878),
               End = c(9.178),
               Height = c(478883),
               Area = c(1939880),
               AreaPerc = c(100),
               HeightPerc = c(100),
               AreaSumPerc = c(100),
               Width = c(0.412),
               FWHM = c(NA),
               StartY = c(0),
               EndY = c(564),
               StartBLY = c(0),
               EndBLY = c(0),
               Symmetry = c(NA),
               MaxY = c(478883),
               SNR = c(NA),
               HeightPerc.Norm. = c(NA),
               Baseline = c('BV'),
               BasePeak = c(319.8622),
               k. = c(NA),
               Label = c(NA),
               Cpd = c(NA),
               Type = c(NA),
               Plates = c(NA),
               Plates.M = c(NA),
               Resolution = c(NA),
               Saturated = c(NA),
               Tailingfactor = c(NA),
               Flags.Tgt. = c(NA),
               RI = c(NA),
               X = c(NA))
  )
  expect_equal(
    result$data[[5]],
    data.frame(Filename = c('D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d ',
                            'D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d '),
               Peak = c(1,2),
               Start = c(14.326,55.278),
               RT = c(14.458,55.374),
               End = c(14.772,55.476),
               Height = c(495962,  2839),
               Area = c(2422859,   7867),
               AreaPerc = c(100.00,  0.32),
               HeightPerc = c(100.00,  0.57),
               AreaSumPerc = c(99.68, 0.32),
               Width = c(0.445,0.198),
               FWHM = c(NA,NA),
               StartY = c(0,0),
               EndY = c(1044,   0),
               StartBLY = c(0,0),
               EndBLY = c(0,0),
               Symmetry = c(NA,NA),
               MaxY = c(495962,  2839),
               SNR = c(NA,NA),
               HeightPerc.Norm. = c(NA,NA),
               Baseline = c('BV','BB'),
               BasePeak = c(339.5537,338.3371),
               k. = c(NA,NA),
               Label = c(NA,NA),
               Cpd = c(NA,NA),
               Type = c(NA,NA),
               Plates = c(NA,NA),
               Plates.M = c(NA,NA),
               Resolution = c(NA,NA),
               Saturated = c(NA,NA),
               Tailingfactor = c(NA,NA),
               Flags.Tgt. = c(NA,NA),
               RI = c(NA,NA),
               X = c(NA,NA))
  )
})

test_that("readPeaklist.AgilentExport works", {
  demoFile <- fs::path_package("extdata", "raw/WorklistData-0005_MS.csv", package = "AgilentDataV2")
  result <- readPeaklist.AgilentExport(demoFile,
                                       startsString = "WorklistData-0005")()
  expect_length(result, 2)
  expect_equal(names(result), c("info", "data"))
  expect_equal(nrow(result$info), 35)
  expect_equal(nrow(result$info), length(result$data))
  expect_equal(result$data[[1]], NA)
  expect_equal(
    result$data[[4]],
    data.frame(Filename = c('D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d '),
               Peak = c(1),
               Start = c(8.766),
               RT = c(8.878),
               End = c(9.178),
               Height = c(478883),
               Area = c(1939880),
               AreaPerc = c(100),
               HeightPerc = c(100),
               AreaSumPerc = c(100),
               Width = c(0.412),
               FWHM = c(NA),
               StartY = c(0),
               EndY = c(564),
               StartBLY = c(0),
               EndBLY = c(0),
               Symmetry = c(NA),
               MaxY = c(478883),
               SNR = c(NA),
               HeightPerc.Norm. = c(NA),
               Baseline = c('BV'),
               BasePeak = c(319.8622),
               k. = c(NA),
               Label = c(NA),
               Cpd = c(NA),
               Type = c(NA),
               Plates = c(NA),
               Plates.M = c(NA),
               Resolution = c(NA),
               Saturated = c(NA),
               Tailingfactor = c(NA),
               Flags.Tgt. = c(NA),
               RI = c(NA),
               X = c(NA))
  )
  expect_equal(
    result$data[[5]],
    data.frame(Filename = c('D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d ',
                            'D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d '),
               Peak = c(1,2),
               Start = c(14.326,55.278),
               RT = c(14.458,55.374),
               End = c(14.772,55.476),
               Height = c(495962,  2839),
               Area = c(2422859,   7867),
               AreaPerc = c(100.00,  0.32),
               HeightPerc = c(100.00,  0.57),
               AreaSumPerc = c(99.68, 0.32),
               Width = c(0.445,0.198),
               FWHM = c(NA,NA),
               StartY = c(0,0),
               EndY = c(1044,   0),
               StartBLY = c(0,0),
               EndBLY = c(0,0),
               Symmetry = c(NA,NA),
               MaxY = c(495962,  2839),
               SNR = c(NA,NA),
               HeightPerc.Norm. = c(NA,NA),
               Baseline = c('BV','BB'),
               BasePeak = c(339.5537,338.3371),
               k. = c(NA,NA),
               Label = c(NA,NA),
               Cpd = c(NA,NA),
               Type = c(NA,NA),
               Plates = c(NA,NA),
               Plates.M = c(NA,NA),
               Resolution = c(NA,NA),
               Saturated = c(NA,NA),
               Tailingfactor = c(NA,NA),
               Flags.Tgt. = c(NA,NA),
               RI = c(NA,NA),
               X = c(NA,NA))
  )
})

test_that("combinePeakList.AgilentExport works",{
  demoFile <- fs::path_package("extdata", "raw/WorklistData-0005_MS.csv", package = "AgilentDataV2")
  result <- readPeaklist.AgilentExport(demoFile, startsString = "WorklistData-0005")()
  expect_equal(combinePeakList.AgilentExport(result) |> head(5),
               data.frame(polarity = '+',
                          signal = 'ESI',
                          scan = 'EIC',
                          mz = c(319.8662,339.5587,339.5587,372.8974,372.8974),
                          MS = 'Scan',
                          fragmentor = 125,
                          filename = 'WorklistData-0005.d',
                          smooth = FALSE,
                          location = 'D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d',
                          start = c( 8,12,12,17,17),
                          end = c(10,15,15,23,23),
                          Filename = 'D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d ',
                          Peak = c(1,1,2,1,2),
                          Start = c( 8.766,14.326,55.278,16.900,17.742),
                          RT = c( 8.878,14.458,55.374,16.917,17.897),
                          End = c( 9.178,14.772,55.476,17.049,17.989),
                          Height = c(478883,495962,  2839,   733,  7371),
                          Area = c(1939880,2422859,   7867,   2401,  36241),
                          AreaPerc = c(100.00,100.00,  0.32,  1.88, 28.32),
                          HeightPerc = c(100.00,100.00,  0.57,  3.14, 31.59),
                          AreaSumPerc = c(100.00, 99.68,  0.32,  0.80, 12.00),
                          Width = c(0.412,0.445,0.198,0.149,0.247),
                          StartY = c(  0,  0,  0,214,  0),
                          EndY = c( 564,1044,   0,  74, 630),
                          StartBLY = c( 0, 0, 0,74, 0),
                          EndBLY = c( 0, 0, 0,74, 0),
                          MaxY = c(478883,495962,  2839,   807,  7371),
                          Baseline = c('BV','BV','BB','VB','BV'),
                          BasePeak = c(319.8622,339.5537,338.3371,371.0429,791.3604)))
})

test_that("getData.MSPeakFiles works",{
  demoFile <- fs::path_package("extdata", "raw/WorklistData-0005_MS.csv", package = "AgilentDataV2")
  result <- getData.MSPeakFiles(filename = demoFile,
                                startsString = "WorklistData-0005")
  expect_equal(result |> head(5),
               data.frame(polarity = '+',
                          signal = 'ESI',
                          scan = 'EIC',
                          mz = c(319.8662,339.5587,339.5587,372.8974,372.8974),
                          MS = 'Scan',
                          fragmentor = 125,
                          filename = 'WorklistData-0005.d',
                          smooth = FALSE,
                          location = 'D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d',
                          start = c( 8,12,12,17,17),
                          end = c(10,15,15,23,23),
                          Filename = 'D:/Projects/GmP Projects/2025-ATS-437/Data/20250916/WorklistData-0005.d ',
                          Peak = c(1,1,2,1,2),
                          Start = c( 8.766,14.326,55.278,16.900,17.742),
                          RT = c( 8.878,14.458,55.374,16.917,17.897),
                          End = c( 9.178,14.772,55.476,17.049,17.989),
                          Height = c(478883,495962,  2839,   733,  7371),
                          Area = c(1939880,2422859,   7867,   2401,  36241),
                          AreaPerc = c(100.00,100.00,  0.32,  1.88, 28.32),
                          HeightPerc = c(100.00,100.00,  0.57,  3.14, 31.59),
                          AreaSumPerc = c(100.00, 99.68,  0.32,  0.80, 12.00),
                          Width = c(0.412,0.445,0.198,0.149,0.247),
                          StartY = c(  0,  0,  0,214,  0),
                          EndY = c( 564,1044,   0,  74, 630),
                          StartBLY = c( 0, 0, 0,74, 0),
                          EndBLY = c( 0, 0, 0,74, 0),
                          MaxY = c(478883,495962,  2839,   807,  7371),
                          Baseline = c('BV','BV','BB','VB','BV'),
                          BasePeak = c(319.8622,339.5537,338.3371,371.0429,791.3604)))
})
