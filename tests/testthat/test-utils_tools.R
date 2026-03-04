test_that("ifelseProper", {
  expect_equal(purrr::map(mtcars$cyl,
                          ~ifelseProper(.x == 6,
                                        list("Six", TRUE),
                                        list("Other", FALSE)))[[1]],
               list("Six", TRUE))
  expect_equal(purrr::map(mtcars$cyl,
                          ~ifelseProper(.x == 6,
                                        list("Six", TRUE),
                                        list("Other", FALSE)))[[3]],
               list("Other", FALSE))
})

test_that("readFile works", {
  filename <- tempfile()
  write.table(mtcars, filename, col.names = TRUE, row.names = TRUE, sep = ";")
  result <- readFile(filename = filename, readFunction = read.csv)(header = TRUE, sep = ";")
  for (counter in 1:ncol(result)){
    result[,counter] <- as.numeric(result[,counter])
  }
  expect_true(identical(result, mtcars))
  file.remove(filename)
  # result <- readFile(filename = filename, readFunction = read.csv)(header = TRUE, sep = ";")
  # expect_true(identical(result, NA))
})

test_that("formatDigitsLargeNumbers works",{
  expect_equal(formatDigitsLargeNumbers(4)(319.65897),"319.6590")
  expect_equal(formatDigitsLargeNumbers(5)(-6.320),"-6.32000")
})

test_that("fieldsPresent works",{
  testList <- list(
    mtcars = datasets::mtcars,
    iris = datasets::iris,
    test = NA
  )
  expect_true(fieldsPresent(theList = testList, fields = c("mtcars", "iris")))
  expect_false(fieldsPresent(theList = testList, fields = c("mtcars", "airquality")))
  expect_false(fieldsPresent(theList = testList, fields = c("mtcars", "test")))
  expect_true(fieldsPresent(theList = testList, fields = c("mtcars", "test"), allowNA = TRUE))
})

test_that("selectStrings works",{
  expect_equal(
    selectStrings(strings = c("test_MS.csv", "testMS.csv"),
                  positive = "_MS.csv$"),
    "test_MS.csv"
  )
  expect_equal(
    selectStrings(strings = c("test_MS.csv", "testMS.csv"),
                  positive = "MS.csv$"),
    c("test_MS.csv", "testMS.csv")
  )
  expect_equal(
    selectStrings(strings = c("test_MS.csv", "testMS.csv"),
                  positive = "MS.csv$", negative = "_"),
    "testMS.csv"
  )
  expect_equal(
    selectStrings(strings = c("test_MS.csv", "testMS.csv"),
                  positive = "MS.csv$", negative = "_",
                  returnStrings = FALSE),
    2
  )
  expect_equal(
    selectStrings(strings = c("test_MS.csv", "testMS.csv"),
                  positive = "MS.csb$", negative = "_",
                  returnStrings = FALSE),
    integer(0)
  )
  expect_equal(
    selectStrings(strings = c("test_MS.csv", "testMS.csv"),
                  positive = "MS.csb$", negative = "_"),
    character(0)
  )
})

test_that("giveFilePathsFromStrings works",{
  expect_equal(
    giveFilePathsFromStrings("R:/testfiles/test.mzdata.xml"),
    "R:/testfiles/test"
  )
  expect_equal(
    giveFilePathsFromStrings(c("R:/testfiles/test.xml",
                               "R:/test2/test5.csv",
                               "R:/testfiles/test.mzdata.xml")),
    c("R:/testfiles/test", "R:/test2/test5")
  )
  expect_equal(
    giveFilePathsFromStrings(c("R:/testfiles/test.xml",
                               "R:/test2/test5.csv",
                               "R:/testfiles/test6.mzdata.xml")),
    c("R:/testfiles/test", "R:/test2/test5", "R:/testfiles/test6")
  )
})
