context("Test mapping tools")

test_that("est eq_map gives the right object", {
  # data
  fp <- system.file("data", "earthquake.csv", package = "capsmsdr")
  signif <- read_tsv(fp) %>%
    eq_clean_date %>%
    eq_clean_location

  # plot
  p <- signif %>%
    filter(COUNTRY == 'BRAZIL' & year(date) >= 2000) %>%
    mutate(popup_text = eq_create_label(.)) %>%
    eq_map()

  # object
  expect_is(p, class = c("leaflet", "htmlwidget"))
})
