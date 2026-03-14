test_that("locationSort works",{
  expect_equal(
    locationSort(
      locations = c("277/279", "192" ,"55")
    ),
    c(277, 192, 55)
  )
})

test_that("ModificationSort works",{
  expect_equal(
    ModificationSort(c("DON82","","DON100", "DON")),
    c(3, 0, 2, 1)
  )
})