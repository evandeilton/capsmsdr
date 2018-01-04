context("Test cleaning functions")

test_that("cleaning functions give right classes", {
  # data
  fp <- system.file("exdata", "earthquake.csv", package = "capsmsdr")
  da <- read_delim(fp, delim = "\t") %>%
    eq_clean_date %>%
    eq_clean_location

  # object classes
  expect_is(da$DATE, class = 'Date')
  expect_is(da$LOCATION, class = 'character')
})
