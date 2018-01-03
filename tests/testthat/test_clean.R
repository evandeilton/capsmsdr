context("Test cleaning functions")

test_that("cleaning functions give right classes", {
  # data
  fp <- system.file("data", "earthquake.csv", package = "capsmsdr")
  signif <- read_tsv(fp) %>%
    eq_clean_date %>%
    eq_clean_location

  # object classes
  expect_is(signif$DATE, class = 'Date')
  expect_is(signif$LOCATION, class = 'character')
})
