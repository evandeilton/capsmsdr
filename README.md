# The capsmsdr package

This is a simple vignette for the packege capsmsdr, a project for the capstone for Coursera's mastering software development in R specilaization.
The general idea is to read and transfor a data set from earthquakes from NOOA site. After get and clean data, we must buil a set of vizualizin functions and geos
to create a timeline vizualization os the data.
We adopt the tidy pardigm for whole package.

You can see Travis package test from [![Travis-CI Build Status](https://travis-ci.org/evandeilton/capsmsdr.svg?branch=master)](https://travis-ci.org/evandeilton/capsmsdr)

## Inatall package

```{r chunk0, eval=FALSE}
devtools::install_github('evandeilton/capsmsdr')
```

# Get and clean data 

## Require packages

```{r chunk00}
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(capsmsdr)
```

## Data set

We put a copy of the data inside our packege to better. No editing was made in original data.

Read data from system folder where the packahe where installed and clear dates and location.

```{r chunk01}
fp <- system.file("exdata", "earthquake.csv", package = "capsmsdr")
df <- read_tsv(fp) %>%
  eq_clean_date %>%
  eq_clean_location
```

View the head of data.

```{r chunk02}
df %>%
  select(DATE, LOCATION, COUNTRY, DEATHS, EQ_PRIMARY) %>% head()
```

## Usage

We show earthquak occured in Mexico between 2000 and 2010. Points represents unique events and sizes are related with the magnitude of the earthquak event. The colors are linked to total deaths registered in the place.

```{r chunk03, fig.align='center', fig.width = 8, fig.height=6}
df %>%
  filter(COUNTRY == 'MEXICO') %>%
  ggplot(aes(date = DATE,
               xmin = as.Date('2000-01-01'),
               xmax = as.Date('2010-12-30'),
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
```

Plot for multiple countries

```{r chunk04, fig.align='center', fig.width = 8, fig.height=6}
df %>%
  filter(COUNTRY %in% c('CHILE', 'BRAZIL')) %>%
  ggplot(aes(date = DATE,
               xmin = as.Date('2000-12-31'),
               xmax = as.Date('2015-12-31'),
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
```


# Interactive maps

If no deaths are computed do infor apperas in the labels. You can zoom map for better vizualisation and more details.

```{r mapping, fig.align='center', fig.width=8, fig.height=8}
df %>%
  filter(COUNTRY == 'CHILE' & year(DATE) >= 2005) %>%
  mutate(popup_text = eq_create_label(.)) %>%
  eq_map()
```


