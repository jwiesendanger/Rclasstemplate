# =====================================================================
# M6A: Wrangling and Tidying Data -- textbook walkthrough
# https://spencergreenhalgh.com/ict_lis_661_textbook_2025_fall/_book/m6a-wrangling-and-tidying-data
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first so here() finds the data.
# Run line by line (Ctrl+Enter) or all at once (Ctrl+Shift+S).
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

# ---------------------------------------------------------------------
# 18.2 Wrangling -- needed packages
# ---------------------------------------------------------------------
status("18.2 Loading packages")
library(dplyr)
library(ggplot2)
library(nycflights23)

# ---------------------------------------------------------------------
# 18.2.1 The pipe operator |>
# ---------------------------------------------------------------------
status("18.2.1 The pipe operator")
envoy_flights <- flights |>
  filter(carrier == "AS")    # note: "AS" is Alaska Airlines; Envoy is "MQ"

# ---------------------------------------------------------------------
# 18.2.2 filter rows
# ---------------------------------------------------------------------
status("18.2.2 filter() rows")
phoenix_flights <- flights |>
  filter(dest == "PHX")
View(phoenix_flights)

btv_sea_flights_fall <- flights |>
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & month >= 10)

# same result, commas instead of &
btv_sea_flights_fall <- flights |>
  filter(origin == "JFK", (dest == "BTV" | dest == "SEA"), month >= 10)
View(btv_sea_flights_fall)

not_BTV_SEA <- flights |>
  filter(!(dest == "BTV" | dest == "SEA"))

many_airports <- flights |>
  filter(dest == "SEA" | dest == "SFO" | dest == "PHX" |
         dest == "BTV" | dest == "BDL")

many_airports <- flights |>
  filter(dest %in% c("SEA", "SFO", "PHX", "BTV", "BDL"))

c(phoenix = nrow(phoenix_flights), btv_sea_fall = nrow(btv_sea_flights_fall),
  not_btv_sea = nrow(not_BTV_SEA), many_airports = nrow(many_airports))

# ---------------------------------------------------------------------
# 18.2.3 mutate existing variables
# ---------------------------------------------------------------------
status("18.2.3 mutate() new variables")
weather <- weather |>
  mutate(temp_in_C = (temp - 32) / 1.8)

flights <- flights |>
  mutate(gain = dep_delay - arr_delay)

flights |> select(dep_delay, arr_delay, gain) |> head(5)   # Table 18.1

flights <- flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )

# ---------------------------------------------------------------------
# 18.2.4 arrange and sort rows
# ---------------------------------------------------------------------
status("18.2.4 arrange() rows")
freq_dest <- flights |>
  group_by(dest) |>
  summarize(num_flights = n())
freq_dest

freq_dest |>
  arrange(num_flights)

freq_dest |>
  arrange(desc(num_flights))

# ---------------------------------------------------------------------
# 18.2.5 join data frames
# ---------------------------------------------------------------------
status("18.2.5 join data frames")
# 18.2.5.1 Matching key variable names
flights_joined <- flights |>
  inner_join(airlines, by = "carrier")

# 18.2.5.2 Different key variable names
flights_with_airport_names <- flights |>
  inner_join(airports, by = c("dest" = "faa"))

named_dests <- flights |>
  group_by(dest) |>
  summarize(num_flights = n()) |>
  arrange(desc(num_flights)) |>
  inner_join(airports, by = c("dest" = "faa")) |>
  rename(airport_name = name)
named_dests

# 18.2.5.3 Multiple key variables
flights_weather_joined <- flights |>
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))

# ---------------------------------------------------------------------
# 18.2.6 Normal forms
# ---------------------------------------------------------------------
status("18.2.6 Normal forms")
joined_flights <- flights |>
  inner_join(airlines, by = "carrier")

# ---------------------------------------------------------------------
# 18.2.7 Other verbs: select, relocate, rename
# ---------------------------------------------------------------------
status("18.2.7 select(), relocate(), rename()")
glimpse(flights)

flights |>
  select(carrier, flight)

flights_no_year <- flights |> select(-year)

flight_arr_times <- flights |> select(month:day, arr_time:sched_arr_time)
flight_arr_times

flights |> select(starts_with("a"))
flights |> select(ends_with("delay"))
flights |> select(contains("time"))

flights_reorder <- flights |>
  select(year, month, day, hour, minute, time_hour, everything())
glimpse(flights_reorder)

flights_relocate <- flights |>
  relocate(hour, minute, time_hour, .after = day)
glimpse(flights_relocate)

flights_time_new <- flights |>
  select(dep_time, arr_time) |>
  rename(departure_time = dep_time, arrival_time = arr_time)
glimpse(flights_time_new)

# ---------------------------------------------------------------------
# 18.2.8 top_n values of a variable
# ---------------------------------------------------------------------
status("18.2.8 top_n()")
named_dests |> top_n(n = 10, wt = num_flights)

named_dests |>
  top_n(n = 10, wt = num_flights) |>
  arrange(desc(num_flights))

# ---------------------------------------------------------------------
# 18.3 Tidying Data -- needed packages
# ---------------------------------------------------------------------
status("18.3 Loading tidying packages")
library(dplyr)
library(readr)
library(tidyr)
library(nycflights23)
library(fivethirtyeight)
library(here)

# ---------------------------------------------------------------------
# 18.3.1 Importing data
# ---------------------------------------------------------------------
status("18.3.1 Importing dem_score.csv")
dem_score <- read_csv(here("activity_data", "dem_score.csv"))
dem_score

# ---------------------------------------------------------------------
# 18.3.2 "Tidy" data
# ---------------------------------------------------------------------
status("18.3.2 Tidy data: drinks")
drinks |> head(5)

drinks_smaller <- drinks |>
  filter(country %in% c("USA", "China", "Italy", "Saudi Arabia")) |>
  select(-total_litres_of_pure_alcohol) |>
  rename(beer = beer_servings, spirit = spirit_servings, wine = wine_servings)
drinks_smaller

drinks_smaller_tidy <- drinks_smaller |>
  gather(type, servings, -country)       # older tidyr function
drinks_smaller_tidy

# ---------------------------------------------------------------------
# 18.3.4 Converting to "tidy" data
# ---------------------------------------------------------------------
status("18.3.4 pivot_longer()")
drinks_smaller

drinks_smaller_tidy <- drinks_smaller |>
  pivot_longer(names_to = "type",
               values_to = "servings",
               cols = -country)
drinks_smaller_tidy

drinks_smaller |>
  pivot_longer(names_to = "type",
               values_to = "servings",
               cols = c(beer, spirit, wine))

drinks_smaller |>
  pivot_longer(names_to = "type",
               values_to = "servings",
               cols = beer:wine)

# ---------------------------------------------------------------------
# 18.3.5 Case study: Democracy in Guatemala
# ---------------------------------------------------------------------
status("18.3.5 Case study: Democracy in Guatemala")
guat_dem <- dem_score |>
  filter(country == "Guatemala")
guat_dem

guat_dem_tidy <- guat_dem |>
  pivot_longer(names_to = "year",
               values_to = "democracy_score",
               cols = -country,
               names_transform = list(year = as.integer))
guat_dem_tidy

# ---------------------------------------------------------------------
# 18.4 tidyverse package
# ---------------------------------------------------------------------
status("18.4 tidyverse package")
library(tidyverse)

View(guat_dem_tidy)
status("Whole M6A walkthrough ran. Results are in the console and viewer.",
       state = "COMPLETE")
