context("Testing cleaning functions")

test_that("cleaning functions give right classes", {
  # load data
  file_path <- system.file("extdata", "signif.tsv", package = "MSDR")
  signif <- read_tsv(file_path) %>%
    eq_clean_date %>%
    eq_clean_location

  # object classes
  expect_is(signif$date, class = 'Date')
  expect_is(signif$LOCATION, class = 'character')
})
