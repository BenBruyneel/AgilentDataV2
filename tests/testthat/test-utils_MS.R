test_that("ppm mass deviation calculations work", {
  expect_equal(
    format(
      calculate.ppm(465.6025, 465.6028),
      digits = 12, nsmall = 8),
    "0.644326437211"
  )
  expect_equal(
    format(
      calculate.ppm(465.6025, 465.6025),
      digits = 12, nsmall = 8),
    "0.00000000"
  )
  expect_equal(
    format(
      calculate.ppm(465.6025, 465.7025),
      digits = 12, nsmall = 8),
    "214.775479083"
  )
  expect_equal(
    format(
      calculate.Measured.mz(465.6025, 5),
      digits = 12, nsmall = 8),
    "465.604828013"
  )
  expect_equal(
    format(
      calculate.Measured.mz(465.6025, 0),
      digits = 12, nsmall = 8),
    "465.60250000"
  )
  expect_equal(
    format(
      calculate.Measured.mz(465.6025, -5),
      digits = 12, nsmall = 8),
    "465.600171988"
  )
})
