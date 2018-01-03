
context("Test visualization tools")

test_that("ploting functions gives the right objects", {
  # data
  fp <- system.file("data", "earthquake.csv", package = "capsmsdr")
  signif <- read_tsv(fp) %>%
    eq_clean_date %>%
    eq_clean_location

  # plot
  p <- signif %>%
    filter(COUNTRY == 'BRAZIL') %>%
    ggplot(aes(date = DATE,
               xmin = as.Date('2000-01-01'),
               xmax = as.Date('2015-12-30'),
               y = COUNTRY,
               colour = DEATHS,
               fill = DEATHS,
               size = EQ_PRIMARY,
               location = LOCATION)) +
    geom_timeline() +
    geom_timeline_label() +
    theme(axis.line.y = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'bottom',
          panel.grid = element_blank(),
          panel.background = element_blank())

  # plot
  expect_is(p, class = c('gg', 'ggplot'))
})


test_that("ploting function passes the right mapping aes", {
  # data
  fp <- system.file("data", "earthquake.csv", package = "capsmsdr")
  signif <- read_tsv(fp) %>%
    eq_clean_date %>%
    eq_clean_location

  # make plot
  p <- signif %>%
    filter(COUNTRY == 'BRAZIL') %>%
    ggplot(aes(date = DATE,
               xmin = as.Date('2000-01-01'),
               xmax = as.Date('2015-12-30'),
               y = COUNTRY,
               colour = DEATHS,
               fill = DEATHS,
               size = EQ_PRIMARY,
               location = LOCATION)) +
    geom_timeline() +
    geom_timeline_label() +
    theme(axis.line.y = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'bottom',
          panel.grid = element_blank(),
          panel.background = element_blank())

  # mapping objects
  expect_identical(p$mapping$y, as.name('COUNTRY'))
  expect_identical(p$mapping$DATE, as.name('DATE'))
  expect_identical(p$mapping$colour, as.name('DEATHS'))
  expect_identical(p$mapping$fill, as.name('DEATHS'))
  expect_identical(p$mapping$size, as.name('EQ_PRIMARY'))
  expect_identical(p$mapping$location, as.name('LOCATION'))
})
