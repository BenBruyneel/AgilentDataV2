test_that("processingFilesEmpty works",{
  expect_equal(processingFilesEmpty(),
               list(
                 MSPeakFiles = as.character(),
                 Spectra = as.character()
               ))
})

test_that("emptyFiles works",{
  expect_true(emptyFiles())
  processingFiles <- list(
    MSPeakFiles = c("WorklistData-0003_MS.csv",
                    "WorklistData-0005_MS.csv"),
    Spectra = c("WorklistData-0003.mzdata.xml",
                "WorklistData-0005.mzdata.xml")
  )
  expect_false(emptyFiles(theFiles = processingFiles))
  expect_true(emptyFiles(theFiles = processingFilesEmpty()))
})

test_that("testCompleteData works",{
  processingFiles <- list(
    MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
                    "./20250702/WorklistData-0005_MS.csv"),
    Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
                "./20250702/WorklistData-0005.mzdata.xml")
  )
  expect_equal(testCompleteData(processingFiles),
               c(
                 "WorklistData-0003" = TRUE,
                 "WorklistData-0005" = TRUE
               ))
  processingFilesError1 <- list(
    MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv"),
    Spectra = c("./20250702/20250702/WorklistData-0003.mzdata.xml",
                "./20250702/WorklistData-0005.mzdata.xml")
  )
  expect_equal(testCompleteData(processingFilesError1),
               c(
                 "WorklistData-0003" = TRUE,
                 "WorklistData-0005" = FALSE
               ))
  processingFilesError2 <- list(
    MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
                    "./20250702/WorklistData-0005_MS.csv"),
    Spectra = c("./20250702/WorklistData-0005.mzdata.xml")
  )
  expect_equal(testCompleteData(processingFilesError2),
               c(
                 "WorklistData-0003" = FALSE,
                 "WorklistData-0005" = TRUE
               ))
})

test_that("gatherRelatedFiles works",{
  processingFiles <- list(
    MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
                    "./20250702/WorklistData-0005_MS.csv"),
    Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
                "./20250702/WorklistData-0005.mzdata.xml")
  )
  expect_equal(
    gatherRelatedFiles(theFiles = processingFiles,
                       sample = "WorklistData-0005"),
    list(
      MSPeakFiles = "./20250702/WorklistData-0005_MS.csv",
      Spectra = "./20250702/WorklistData-0005.mzdata.xml"
    )
  )
  expect_equal(
    gatherRelatedFiles(theFiles = processingFiles,
                       sample = "WorklistData-0003"),
    list(
      MSPeakFiles = "./20250702/WorklistData-0003_MS.csv",
      Spectra = "./20250702/WorklistData-0003.mzdata.xml"
    )
  )
})

test_that("gatherAllCompleteFiles works",{
  processingFiles <- list(
    MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
                    "./20250702/WorklistData-0005_MS.csv"),
    Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
                "./20250702/WorklistData-0005.mzdata.xml")
  )
  processingFilesError1 <- list(
    MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv"),
    Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
                "./20250702/WorklistData-0005.mzdata.xml")
  )
  processingFilesError2 <- list(
    MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
                    "./20250702/WorklistData-0005_MS.csv"),
    Spectra = c("./20250702/WorklistData-0005.mzdata.xml")
  )
  expect_equal(
    gatherAllCompleteFiles(processingFiles),
    list(
      MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
                      "./20250702/WorklistData-0005_MS.csv"),
      Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
                  "./20250702/WorklistData-0005.mzdata.xml")
    )
  )
  expect_equal(
    gatherAllCompleteFiles(processingFilesError1),
    list(
      MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv"),
      Spectra = c("./20250702/WorklistData-0003.mzdata.xml")
    )
  )
  expect_equal(
    gatherAllCompleteFiles(processingFilesError2),
    list(
      MSPeakFiles = c("./20250702/WorklistData-0005_MS.csv"),
      Spectra = c("./20250702/WorklistData-0005.mzdata.xml")
    )
  )
})

test_that("countProcessingData works", {
  processingFiles <- list(
    MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
                    "./20250702/WorklistData-0005_MS.csv"),
    Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
                "./20250702/WorklistData-0005.mzdata.xml")
  )
  processingFilesError1 <- list(
    MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv"),
    Spectra = c("./20250702/20250702/WorklistData-0003.mzdata.xml",
                "./20250702/WorklistData-0005.mzdata.xml")
  )
  expect_equal(countProcessingData(processingFiles), 2)
  expect_equal(countProcessingData(processingFilesError1), 1)
  expect_equal(countProcessingData(processingFilesError1,
                                   includeIncomplete = TRUE), 2)

})
