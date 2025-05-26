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

#try to create new dataset

logisticData553 <- Ratios_2 |>
  #We only want commercial planes, fixed wing aircraft, with turbofan engines
  filter(TYPE_AIRCRAFT == 5, TYPE_REGISTRANT == 3, TYPE_ENGINE == 5) |>
  filter(YEAR_MFR==2008)

# View(logisticData553)

log553Expanded <- logisticData553 |>
  rowwise() |>
  mutate(
    delayed_flags = list(c(rep(1, COUNT_LATE_AIRCRAFT_DELAY_30), rep(0, FLIGHTS_ANNUAL - COUNT_LATE_AIRCRAFT_DELAY_30)))
                         ) |>
  unnest_longer(delayed_flags, values_to = "delayed30") |>
  ungroup()

# View(log553Expanded)

Logistic553model <- glm(delayed30 ~ AIR_TIME_SD + AGE_SD + FLIGHTS_SD + DISTANCE_SD, data = log553Expanded, family = binomial)
Logistic553model  
