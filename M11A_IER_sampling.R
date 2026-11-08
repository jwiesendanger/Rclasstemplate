# =====================================================================
# M11A: Sampling -- applied to OUR IER sample data
# Data: activity_data/ier_deidentified.csv.gz (de-identified IER export)
#
# Mirrors the textbook steps. The population is every individual send
# (384,024 rows), so here we know the true answers and can watch samples
# try to recover them:
#   33.1  red balls     -> opened sends: sample proportion of sends opened
#   33.4  almond weight -> send hour (time of day, 0-24): sample mean
#   33.5  two samples   -> open rate, UK recipients minus external recipients
# The class's 33 hand-drawn samples (33.1.2) have no IER equivalent, so the
# 33 virtual samples stand in for them.
#
# Every object ends in _ier so nothing from the textbook walkthrough
# (M11A_walkthrough.R) is overwritten.
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# Plots show in the Plots pane and are saved to m11a_ier_plots/.
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

dir.create("m11a_ier_plots", showWarnings = FALSE)
show <- function(p, name) {
  print(p)
  ggplot2::ggsave(file.path("m11a_ier_plots", paste0(name, ".png")), p,
                  width = 8, height = 5, dpi = 100, bg = "white")
  invisible(p)
}

status("Loading packages and IER data")
library(tidyverse)
library(moderndive)
library(infer)
library(here)
set.seed(76)

# The population: one row per individual send
sends_ier <- read_csv(here("activity_data", "ier_deidentified.csv.gz"),
                      show_col_types = FALSE) |>
  transmute(send_ID          = row_number(),
            email_name       = `Individual Email Result: Email Name`,
            opened           = Opened,
            recipient_domain = recipient_domain,
            sent_at          = as.POSIXct(`Date Sent`,
                                          format = "%m/%d/%Y, %I:%M %p", tz = "UTC"),
            send_hour        = as.numeric(format(sent_at, "%H")) +
              as.numeric(format(sent_at, "%M")) / 60,
            audience         = if_else(recipient_domain %in% c("uky.edu", "email.uky.edu"),
                                       "UK", "External"))

# =====================================================================
# 33.1 First activity: opened sends (the "red balls")
# 33.1.1 The proportion of sends opened in the population
# =====================================================================
status("33.1.1 Proportion of all sends that were opened")
sends_ier

sends_ier |>
  mutate(is_opened = (opened == 1))

sends_ier |>
  mutate(is_opened = (opened == 1)) |>
  summarize(num_opened = sum(is_opened))

sends_ier |>
  mutate(is_opened = (opened == 1)) |>
  summarize(prop_opened = mean(is_opened))

sends_ier |>
  summarize(prop_opened = mean(opened == 1))

# =====================================================================
# 33.1.3 Virtual sampling
# =====================================================================
status("33.1.3 One virtual sample of 50 sends")
virtual_shovel_ier <- sends_ier |>
  rep_slice_sample(n = 50)
virtual_shovel_ier

virtual_shovel_ier |>
  summarize(prop_opened = mean(opened == 1))

status("33.1.3 Thirty-three virtual samples")
virtual_samples_ier <- sends_ier |>
  rep_slice_sample(n = 50, reps = 33)
virtual_samples_ier

virtual_prop_opened_33_ier <- virtual_samples_ier |>
  group_by(replicate) |>
  summarize(prop_opened = mean(opened == 1))
virtual_prop_opened_33_ier

show(
  ggplot(virtual_prop_opened_33_ier, aes(x = prop_opened)) +
    geom_histogram(binwidth = 0.05, boundary = 0.2, color = "white") +
    labs(x = "Proportion of 50 sends that were opened",
         title = "Histogram of 33 sample proportions"),
  "1_open_prop_33_samples"
)

status("33.1.3 One thousand virtual samples")
virtual_prop_opened_ier <- sends_ier |>
  rep_slice_sample(n = 50, reps = 1000) |>
  summarize(prop_opened = mean(opened == 1))
virtual_prop_opened_ier

show(
  ggplot(virtual_prop_opened_ier, aes(x = prop_opened)) +
    geom_histogram(binwidth = 0.04, boundary = 0.2, color = "white") +
    labs(x = "Proportion of 50 sends that were opened",
         title = "Histogram of 1000 sample proportions"),
  "2_open_prop_1000_samples"
)

status("33.1.3 Different sample sizes (25, 50, 100)")
# Segment 1: sample size = 25
virtual_prop_opened_25_ier <- sends_ier |>
  rep_slice_sample(n = 25, reps = 1000) |>
  summarize(prop_opened = mean(opened == 1))

show(
  ggplot(virtual_prop_opened_25_ier, aes(x = prop_opened)) +
    geom_histogram(binwidth = 0.05, boundary = 0.2, color = "white") +
    labs(x = "Proportion of 25 sends that were opened", title = "25"),
  "3a_open_prop_n25"
)

# Segment 2: sample size = 50
virtual_prop_opened_50_ier <- sends_ier |>
  rep_slice_sample(n = 50, reps = 1000) |>
  summarize(prop_opened = mean(opened == 1))

show(
  ggplot(virtual_prop_opened_50_ier, aes(x = prop_opened)) +
    geom_histogram(binwidth = 0.05, boundary = 0.2, color = "white") +
    labs(x = "Proportion of 50 sends that were opened", title = "50"),
  "3b_open_prop_n50"
)

# Segment 3: sample size = 100
virtual_prop_opened_100_ier <- sends_ier |>
  rep_slice_sample(n = 100, reps = 1000) |>
  summarize(prop_opened = mean(opened == 1))

show(
  ggplot(virtual_prop_opened_100_ier, aes(x = prop_opened)) +
    geom_histogram(binwidth = 0.05, boundary = 0.2, color = "white") +
    labs(x = "Proportion of 100 sends that were opened", title = "100"),
  "3c_open_prop_n100"
)

# =====================================================================
# 33.3 The Central Limit Theorem
# 33.3.3 The center of the distribution: the expected value
# =====================================================================
status("33.3.3 Expected value of the sample proportion")
virtual_prop_opened_25_ier |>
  summarize(E_Xbar_25 = mean(prop_opened))
virtual_prop_opened_50_ier |>
  summarize(E_Xbar_50 = mean(prop_opened))
virtual_prop_opened_100_ier |>
  summarize(E_Xbar_100 = mean(prop_opened))

# =====================================================================
# 33.3.4 Sampling variation: standard deviation and standard error
# =====================================================================
status("33.3.4 Standard deviation and standard error")
sends_ier |>
  mutate(is_opened = opened == 1) |>
  summarize(p = mean(is_opened), st_dev = sd(is_opened))

p_ier <- mean(sends_ier$opened == 1)   # the true open rate (population)
sqrt(p_ier * (1 - p_ier))

sends_ier |>
  rep_slice_sample(n = 100, replace = TRUE, reps = 10000) |>
  summarize(prop_opened = mean(opened == 1)) |>
  summarize(p = mean(prop_opened), SE_Xbar = sd(prop_opened))

sqrt(p_ier * (1 - p_ier) / 100)

# Simulated vs. formula standard error for n = 25 and n = 50
virtual_prop_opened_25_ier |>
  summarize(SE_Xbar_25 = sd(prop_opened))
virtual_prop_opened_50_ier |>
  summarize(SE_Xbar_50 = sd(prop_opened))

sqrt(p_ier * (1 - p_ier) / 25)
sqrt(p_ier * (1 - p_ier) / 50)

# =====================================================================
# 33.4 Second activity: send hour (the "almond weights")
# 33.4.1 The population mean send hour
# =====================================================================
status("33.4.1 Population mean send hour")
sends_ier |>
  select(send_ID, sent_at, send_hour)

sends_ier |>
  summarize(mean_hour = mean(send_hour),
            sd_hour   = sd(send_hour),
            length    = n())

show(
  ggplot(sends_ier, aes(x = send_hour)) +
    geom_histogram(binwidth = 1, boundary = 0, color = "white") +
    labs(x = "Hour of day the send went out (13 = 1 PM)",
         title = "All 384,024 sends: lopsided, with a spike at 1 PM"),
  "4a_send_hour_population"
)

# =====================================================================
# 33.4.2 One sample and its sample mean
# =====================================================================
status("33.4.2 One sample of 25 sends")
sends_sample_ier <- sends_ier |>
  rep_slice_sample(n = 25)
sends_sample_ier |>
  select(replicate, send_ID, sent_at, send_hour)

show(
  ggplot(sends_sample_ier, aes(x = send_hour)) +
    geom_histogram(binwidth = 1, boundary = 0, color = "white") +
    labs(x = "Hour of day the send went out", title = "One sample of 25 sends"),
  "4b_send_hour_one_sample"
)

sends_sample_ier |> summarize(sample_mean_hour = mean(send_hour))

# =====================================================================
# 33.4.3 Virtual sampling
# =====================================================================
status("33.4.3 1000 virtual samples of 25 sends")
virtual_samples_hour_ier <- sends_ier |>
  rep_slice_sample(n = 25, reps = 1000)
virtual_samples_hour_ier

virtual_mean_hour_ier <- virtual_samples_hour_ier |>
  summarize(mean_hour = mean(send_hour))
virtual_mean_hour_ier

show(
  ggplot(virtual_mean_hour_ier, aes(x = mean_hour)) +
    geom_histogram(binwidth = 0.25, boundary = 13, color = "white") +
    labs(x = "Sample mean send hour", title = "Histogram of 1000 sample means"),
  "4c_send_hour_1000_sample_means"
)

# =====================================================================
# 33.4.6 The Central Limit Theorem revisited
# =====================================================================
status("33.4.6 Sample mean send hour for n = 25, 50, 100")
# Segment 1: sample size = 25
virtual_mean_hour_25_ier <- sends_ier |>
  rep_slice_sample(n = 25, reps = 1000) |>
  summarize(mean_hour = mean(send_hour), n = n())

show(
  ggplot(virtual_mean_hour_25_ier, aes(x = mean_hour)) +
    geom_histogram(binwidth = 0.1, boundary = 13, color = "white") +
    labs(x = "Sample mean send hour for random samples of 25 sends", title = "25"),
  "5a_mean_hour_n25"
)

# Segment 2: sample size = 50
virtual_mean_hour_50_ier <- sends_ier |>
  rep_slice_sample(n = 50, reps = 1000) |>
  summarize(mean_hour = mean(send_hour), n = n())

show(
  ggplot(virtual_mean_hour_50_ier, aes(x = mean_hour)) +
    geom_histogram(binwidth = 0.1, boundary = 13, color = "white") +
    labs(x = "Sample mean send hour for random samples of 50 sends", title = "50"),
  "5b_mean_hour_n50"
)

# Segment 3: sample size = 100
virtual_mean_hour_100_ier <- sends_ier |>
  rep_slice_sample(n = 100, reps = 1000) |>
  summarize(mean_hour = mean(send_hour), n = n())

show(
  ggplot(virtual_mean_hour_100_ier, aes(x = mean_hour)) +
    geom_histogram(binwidth = 0.1, boundary = 13, color = "white") +
    labs(x = "Sample mean send hour for random samples of 100 sends", title = "100"),
  "5c_mean_hour_n100"
)

sends_ier |>
  summarize(mu = mean(send_hour), sigma = sd(send_hour))

virtual_mean_hour_25_ier |>
  summarize(E_Xbar_25 = mean(mean_hour), sd = sd(mean_hour))
virtual_mean_hour_50_ier |>
  summarize(E_Xbar_50 = mean(mean_hour), sd = sd(mean_hour))
virtual_mean_hour_100_ier |>
  summarize(E_Xbar_100 = mean(mean_hour), sd = sd(mean_hour))

# Formula standard error, sigma / sqrt(n), for comparison
sigma_ier <- sd(sends_ier$send_hour)
sigma_ier / sqrt(c(25, 50, 100))

# =====================================================================
# 33.5.1 Sampling distribution for two samples
# Open rate: UK recipients minus external recipients
# =====================================================================
status("33.5.1 Difference in open rate, UK minus external recipients")
sends_ier |>
  group_by(audience) |>
  summarize(sends = n(), open_rate = mean(opened == 1))

virtual_prop_uk_ier <- sends_ier |>
  filter(audience == "UK") |>
  rep_slice_sample(n = 50, reps = 1000) |>
  summarize(prop_uk = mean(opened == 1))
virtual_prop_external_ier <- sends_ier |>
  filter(audience == "External") |>
  rep_slice_sample(n = 50, reps = 1000) |>
  summarize(prop_external = mean(opened == 1))
prop_joined_ier <- virtual_prop_uk_ier |>
  inner_join(virtual_prop_external_ier, by = "replicate") |>
  mutate(prop_diff = prop_uk - prop_external)

prop_joined_ier

show(
  ggplot(prop_joined_ier, aes(x = prop_diff)) +
    geom_histogram(binwidth = 0.04, boundary = 0, color = "white") +
    labs(x = "Difference in sample open rates (UK minus external)",
         title = "Histogram of 1000 differences in sample proportions"),
  "6_open_rate_diff_uk_vs_external"
)

prop_joined_ier |>
  summarize(mean_diff = mean(prop_diff), SE_diff = sd(prop_diff),
            share_below_zero = mean(prop_diff < 0))

status(paste("M11A steps ran on the IER data.", length(list.files("m11a_ier_plots")),
             "plots saved in m11a_ier_plots/."), state = "COMPLETE")
