#library
library(dplyr)
library(readr)
if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")
}
library(purrr)
library(tidyr)

#Using Ratios_2 data

Ratios_2 <- read_csv("Ratios_2.csv")

#SD for age and flights
Ratios_2$AGE_SD       <- scale(Ratios_2$AGE)[, 1]
Ratios_2$FLIGHTS_SD   <- scale(Ratios_2$FLIGHTS)[, 1]

logisticData5E3 <- Ratios_2 |>
  #We only want commercial planes, fixed wing aircraft, several engine options
  filter(TYPE_AIRCRAFT == 5, TYPE_REGISTRANT == 3) |>
  filter(TYPE_ENGINE %in% c(2, 4, 5)) |>
  filter(YEAR_MFR %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014))

# View(logisticData553)

log5E3Expanded <- logisticData5E3 |>
  rowwise() |>
  mutate(
    delayed_flags = list(c(rep(1, COUNT_LATE_AIRCRAFT_DELAY_30), rep(0, FLIGHTS_ANNUAL - COUNT_LATE_AIRCRAFT_DELAY_30)))
  ) |>
  unnest_longer(delayed_flags, values_to = "delayed30") |>
  ungroup()

# View(log553Expanded)

Logistic5E3model <- glm(delayed30 ~ AIR_TIME_SD + AGE_SD + FLIGHTS_SD + DISTANCE_SD, data = log5E3Expanded, family = binomial)

Logistic5E3model  

