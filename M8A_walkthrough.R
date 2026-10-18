# =====================================================================
# M8A: Descriptive Statistics -- textbook walkthrough
# https://spencergreenhalgh.com/ict_lis_661_textbook_2025_fall/_book/m8a-descriptive-statistics.html
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# Run line by line (Ctrl+Enter) or all at once (Ctrl+Shift+S).
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

# ---------------------------------------------------------------------
# Needed packages
# ---------------------------------------------------------------------
status("Loading packages")
library(tidyverse)
library(nycflights23)

# =====================================================================
# 24.2.3 mean() and sd() inside summarize()
# =====================================================================
status("24.2.3 mean() and sd() -- note the NA result first")
summary_windspeed <- weather |>
  summarize(mean = mean(wind_speed), std_dev = sd(wind_speed))
summary_windspeed            # NA, because wind_speed has missing values

summary_windspeed <- weather |>
  summarize(mean = mean(wind_speed, na.rm = TRUE),
            std_dev = sd(wind_speed, na.rm = TRUE))
summary_windspeed            # textbook: 9.43405, 5.26836

# How many missing values were we ignoring?
sum(is.na(weather$wind_speed))

# Other summary functions mentioned in the chapter
weather |>
  summarize(min    = min(wind_speed, na.rm = TRUE),
            max    = max(wind_speed, na.rm = TRUE),
            median = median(wind_speed, na.rm = TRUE),
            sum    = sum(wind_speed, na.rm = TRUE),
            iqr    = IQR(wind_speed, na.rm = TRUE),
            n      = n())

# =====================================================================
# 24.2.4 Percentiles, quartiles and IQR()
# =====================================================================
status("24.2.4 Percentiles, quartiles and IQR")
# The chapter's 14-value example
x <- c(1, 11.5, 6, 7.2, 4, 8, 9, 10, 6.8, 8.3, 2, 2, 10, 1)
sort(x)
median(x)                    # textbook: 7
quantile(x)                  # Q1 (25%), median (50%), Q3 (75%)
IQR(x)                       # Q3 - Q1

# Outlier fences: 1.5 x IQR beyond Q1 / Q3
q <- quantile(x, c(0.25, 0.75))
c(lower_fence = q[[1]] - 1.5 * IQR(x), upper_fence = q[[2]] + 1.5 * IQR(x))

# The "mean +/- 2 sd" rule of thumb, on wind speed
ws <- summary_windspeed
c(two_sd_low = ws$mean - 2 * ws$std_dev, two_sd_high = ws$mean + 2 * ws$std_dev)

# =====================================================================
# 24.3 group_by rows
# =====================================================================
status("24.3 group_by() + summarize()")
summary_monthly_windspeed <- weather |>
  group_by(month) |>
  summarize(mean = mean(wind_speed, na.rm = TRUE),
            std_dev = sd(wind_speed, na.rm = TRUE))
summary_monthly_windspeed

# group_by() alone only changes the meta-data
diamonds
diamonds |>
  group_by(cut)

diamonds |>
  group_by(cut) |>
  summarize(avg_price = mean(price))

diamonds |>
  group_by(cut) |>
  ungroup()

# n() counts rows; sum() adds up a numeric variable
status("24.3 n() vs sum()")
by_origin <- flights |>
  group_by(origin) |>
  summarize(count = n())
by_origin                    # textbook: EWR 138578, JFK 133048, LGA 163726

# Grouping by more than one variable
status("24.3 Grouping by more than one variable")
by_origin_monthly <- flights |>
  group_by(origin, month) |>
  summarize(count = n())
by_origin_monthly            # 36 rows, still grouped by origin

# Incorrect: the second group_by() overwrites the first
by_origin_monthly_incorrect <- flights |>
  group_by(origin) |>
  group_by(month) |>
  summarize(count = n())
by_origin_monthly_incorrect  # only 12 rows -- origin was lost

status("Whole M8A walkthrough ran. Results are in the console above.",
       state = "COMPLETE")
