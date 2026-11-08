# =====================================================================
# M11A: Sampling -- textbook walkthrough
# https://spencergreenhalgh.com/ict_lis_661_textbook_2025_fall/_book/m11a-sampling
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# Run line by line (Ctrl+Enter) or all at once (Ctrl+Shift+S).
# Plots show in the Plots pane and are also saved to m11a_plots/.
# Nothing is removed from the environment. Where the book reuses a name,
# a copy is kept first (lines marked "# keep:") so every result survives.
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

dir.create("m11a_plots", showWarnings = FALSE)
show <- function(p, name) {
  print(p)
  ggplot2::ggsave(file.path("m11a_plots", paste0(name, ".png")), p,
                  width = 8, height = 5, dpi = 100, bg = "white")
  invisible(p)
}

# ---------------------------------------------------------------------
# Needed packages
# ---------------------------------------------------------------------
status("Loading packages")
library(tidyverse)
library(moderndive)
library(infer)

set.seed(76)   # so the random samples below are the same each run

# =====================================================================
# 33.1 First activity: red balls
# 33.1.1 The proportion of red balls in the bowl
# =====================================================================
status("33.1.1 Proportion of red balls in the bowl")
bowl

bowl |>
  mutate(is_red = (color == "red"))

bowl |>
  mutate(is_red = (color == "red")) |>
  summarize(num_red = sum(is_red))

bowl |>
  mutate(is_red = (color == "red")) |>
  summarize(prop_red = mean(is_red))

bowl |>
  summarize(prop_red = mean(color == "red"))

# =====================================================================
# 33.1.2 Manual sampling (the class's 33 tactile samples)
# =====================================================================
status("33.1.2 Manual sampling")
tactile_prop_red

show(
  ggplot(tactile_prop_red, aes(x = prop_red)) +
    geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
    labs(x = "Proportion of red balls in each sample",
         title = "Histogram of 33 proportions"),
  "33-07_tactile_33_proportions"
)

# =====================================================================
# 33.1.3 Virtual sampling
# =====================================================================
status("33.1.3 One virtual sample")
virtual_shovel <- bowl |>
  rep_slice_sample(n = 50)
virtual_shovel

virtual_shovel |>
  summarize(prop_red = mean(color == "red"))

status("33.1.3 Thirty-three virtual samples")
virtual_samples <- bowl |>
  rep_slice_sample(n = 50, reps = 33)
virtual_samples

virtual_prop_red <- virtual_samples |>
  group_by(replicate) |>
  summarize(prop_red = mean(color == "red"))
virtual_prop_red

virtual_prop_red <- bowl |>
  rep_slice_sample(n = 50, reps = 33) |>
  summarize(prop_red = mean(color == "red"))
virtual_prop_red
virtual_prop_red_33 <- virtual_prop_red   # keep: 33-sample version

show(
  ggplot(virtual_prop_red, aes(x = prop_red)) +
    geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
    labs(x = "Sample proportion",
         title = "Histogram of 33 sample proportions"),
  "33-08_virtual_33_proportions"
)

status("33.1.3 One thousand virtual samples")
virtual_prop_red <- bowl |>
  rep_slice_sample(n = 50, reps = 1000) |>
  summarize(prop_red = mean(color == "red"))
virtual_prop_red
virtual_prop_red_1000 <- virtual_prop_red   # keep: 1000-sample version

show(
  ggplot(virtual_prop_red, aes(x = prop_red)) +
    geom_histogram(binwidth = 0.04, boundary = 0.4, color = "white") +
    labs(x = "Sample proportion", title = "Histogram of 1000 sample proportions"),
  "33-10_virtual_1000_proportions"
)

status("33.1.3 Different sample sizes (25, 50, 100)")
# Segment 1: sample size = 25
virtual_prop_red_25 <- bowl |>
  rep_slice_sample(n = 25, reps = 1000) |>
  summarize(prop_red = mean(color == "red"))

show(
  ggplot(virtual_prop_red_25, aes(x = prop_red)) +
    geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
    labs(x = "Proportion of 25 balls that were red", title = "25"),
  "33-12a_prop_red_n25"
)

# Segment 2: sample size = 50
virtual_prop_red_50 <- bowl |>
  rep_slice_sample(n = 50, reps = 1000) |>
  summarize(prop_red = mean(color == "red"))

show(
  ggplot(virtual_prop_red_50, aes(x = prop_red)) +
    geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
    labs(x = "Proportion of 50 balls that were red", title = "50"),
  "33-12b_prop_red_n50"
)

# Segment 3: sample size = 100
virtual_prop_red_100 <- bowl |>
  rep_slice_sample(n = 100, reps = 1000) |>
  summarize(prop_red = mean(color == "red"))

show(
  ggplot(virtual_prop_red_100, aes(x = prop_red)) +
    geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
    labs(x = "Proportion of 100 balls that were red", title = "100"),
  "33-12c_prop_red_n100"
)

# =====================================================================
# 33.3 The Central Limit Theorem
# 33.3.3 The center of the distribution: the expected value
# =====================================================================
status("33.3.3 Expected value of the sample proportion")
virtual_prop_red_25

virtual_prop_red_25 |>
  summarize(E_Xbar_25 = mean(prop_red))
virtual_prop_red_50 |>
  summarize(E_Xbar_50 = mean(prop_red))
virtual_prop_red_100 |>
  summarize(E_Xbar_100 = mean(prop_red))

# =====================================================================
# 33.3.4 Sampling variation: standard deviation and standard error
# =====================================================================
status("33.3.4 Standard deviation and standard error")
bowl |>
  mutate(is_red = color == "red") |>
  summarize(p = mean(is_red), st_dev = sd(is_red))

p <- 0.375
sqrt(p * (1 - p))

bowl |>
  rep_slice_sample(n = 100, replace = TRUE, reps = 10000) |>
  summarize(prop_red = mean(color == "red")) |>
  summarize(p = mean(prop_red), SE_Xbar = sd(prop_red))

sqrt(p * (1 - p) / 100)

# (The book's column names are off by one: these are n = 25 and n = 50.)
virtual_prop_red_25 |>
  summarize(SE_Xbar_50 = sd(prop_red))
virtual_prop_red_50 |>
  summarize(SE_Xbar_100 = sd(prop_red))

sqrt(p * (1 - p) / 25)
sqrt(p * (1 - p) / 50)

# =====================================================================
# 33.4 Second activity: chocolate-covered almonds
# 33.4.1 The population mean weight of almonds in the bowl
# =====================================================================
status("33.4.1 Population mean weight of almonds")
almonds_bowl

almonds_bowl |>
  summarize(mean_weight = mean(weight),
            sd_weight = sd(weight),
            length = n())

show(
  ggplot(almonds_bowl, aes(x = weight)) +
    geom_histogram(binwidth = 0.1, color = "white"),
  "33-4-1_almonds_population"
)

# =====================================================================
# 33.4.2 Manual sampling and sample means
# =====================================================================
status("33.4.2 One sample of 25 almonds")
almonds_sample

show(
  ggplot(almonds_sample, aes(x = weight)) +
    geom_histogram(binwidth = 0.1, color = "white"),
  "33-4-2_almonds_one_sample"
)

almonds_sample |> summarize(sample_mean_weight = mean(weight))

# =====================================================================
# 33.4.3 Virtual sampling
# =====================================================================
status("33.4.3 1000 virtual samples of 25 almonds")
virtual_samples_almonds <- almonds_bowl |>
  rep_slice_sample(n = 25, reps = 1000)
virtual_samples_almonds

virtual_mean_weight <- virtual_samples_almonds |>
  summarize(mean_weight = mean(weight))
virtual_mean_weight

show(
  ggplot(virtual_mean_weight, aes(x = mean_weight)) +
    geom_histogram(binwidth = 0.04, boundary = 3.5, color = "white") +
    labs(x = "Sample mean", title = "Histogram of 1000 sample means"),
  "33-4-3_almonds_1000_sample_means"
)

# =====================================================================
# 33.4.5 Random variables
# =====================================================================
status("33.4.5 Random variables")
almonds_sample
almonds_sample |>
  summarize(sample_mean_weight = mean(weight))

# =====================================================================
# 33.4.6 The Central Limit Theorem revisited
# =====================================================================
status("33.4.6 Sample means for n = 25, 50, 100")
# Segment 1: sample size = 25
virtual_mean_weight_25 <- almonds_bowl |>
  rep_slice_sample(n = 25, reps = 1000) |>
  summarize(mean_weight = mean(weight), n = n())

show(
  ggplot(virtual_mean_weight_25, aes(x = mean_weight)) +
    geom_histogram(binwidth = 0.02, boundary = 3.6, color = "white") +
    labs(x = "Sample mean weights for random samples of 25 almonds", title = "25"),
  "33-4-6a_almond_means_n25"
)

# Segment 2: sample size = 50
virtual_mean_weight_50 <- almonds_bowl |>
  rep_slice_sample(n = 50, reps = 1000) |>
  summarize(mean_weight = mean(weight), n = n())

show(
  ggplot(virtual_mean_weight_50, aes(x = mean_weight)) +
    geom_histogram(binwidth = 0.02, boundary = 3.6, color = "white") +
    labs(x = "Sample mean weights for random samples of 50 almonds", title = "50"),
  "33-4-6b_almond_means_n50"
)

# Segment 3: sample size = 100
virtual_mean_weight_100 <- almonds_bowl |>
  rep_slice_sample(n = 100, reps = 1000) |>
  summarize(mean_weight = mean(weight), n = n())

show(
  ggplot(virtual_mean_weight_100, aes(x = mean_weight)) +
    geom_histogram(binwidth = 0.02, boundary = 3.6, color = "white") +
    labs(x = "Sample mean weights for random samples of 100 almonds", title = "100"),
  "33-4-6c_almond_means_n100"
)

almonds_bowl |>
  summarize(mu = mean(weight), sigma = sd(weight))

virtual_mean_weight_25 |>
  summarize(E_Xbar_25 = mean(mean_weight), sd = sd(mean_weight))
virtual_mean_weight_50 |>
  summarize(E_Xbar_50 = mean(mean_weight), sd = sd(mean_weight))
virtual_mean_weight_100 |>
  summarize(E_Xbar_100 = mean(mean_weight), sd = sd(mean_weight))

# =====================================================================
# 33.5.1 Sampling distribution for two samples
# =====================================================================
status("33.5.1 Difference in two sample proportions")
virtual_prop_red <- bowl |>
  rep_slice_sample(n = 50, reps = 1000) |>
  summarize(prop_red = mean(color == "red"))
virtual_prop_almond <- almonds_bowl |>
  rep_slice_sample(n = 60, reps = 1000) |>
  summarize(prop_almond = mean(weight > 3.8))
prop_joined <- virtual_prop_red |>
  inner_join(virtual_prop_almond, by = "replicate") |>
  mutate(prop_diff = prop_red - prop_almond)

prop_joined

show(
  ggplot(prop_joined, aes(x = prop_diff)) +
    geom_histogram(binwidth = 0.04, boundary = 0, color = "white") +
    labs(x = "Difference in sample proportions",
         title = "Histogram of 1000 differences in sample proportions"),
  "33-5-1_diff_in_proportions"
)

# Everything built in this walkthrough:
ls()

status("M11A walkthrough complete", "DONE")
