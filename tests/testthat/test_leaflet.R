context("Test mapping tools")

test_that("est eq_map gives the right object", {
  # data
  fp <- system.file("exdata", "earthquake.csv", package = "capsmsdr")
  da <- read_delim(fp, delim = "\t", progress = 500) %>%
    eq_clean_date %>%
    eq_clean_location

  # plot
  p <- da %>%
    filter(COUNTRY == 'MEXICO' & year(DATE) >= 2000) %>%
    mutate(popup_text = eq_create_label(.)) %>%
	mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%
	mutate(LATITUDE  = as.numeric(LATITUDE)) %>%
    eq_map()

  # object
  expect_is(p, class = c("leaflet", "htmlwidget"))
})
