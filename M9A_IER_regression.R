# =====================================================================
# M9A: Basic Regression -- applied to OUR IER sample data
# Data: activity_data/ier_deidentified.csv.gz (de-identified IER export)
#
# Mirrors the textbook steps:
#   27.2  one NUMERICAL explanatory variable  -> open rate ~ send size
#   27.3  one CATEGORICAL explanatory variable -> opened ~ domain group
#   27.4  correlation vs. causation, best-fitting line, under the hood
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# Plots show in the Plots pane and are saved to m9a_ier_plots/.
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

dir.create("m9a_ier_plots", showWarnings = FALSE)
show <- function(p, name) {
  print(p)
  ggplot2::ggsave(file.path("m9a_ier_plots", paste0(name, ".png")), p,
                  width = 8, height = 5, dpi = 100, bg = "white")
  invisible(p)
}

status("Loading packages and IER data")
library(tidyverse)
library(moderndive)
library(here)
set.seed(76)

ier <- read_csv(here("activity_data", "ier_deidentified.csv.gz"),
                show_col_types = FALSE) |>
  rename(email_name   = `Individual Email Result: Email Name`,
         from_name    = `From Name`,
         opened       = Opened,
         clicked      = Clicked,
         date_bounced = `Date Bounced`) |>
  mutate(domain_group = case_when(
    recipient_domain %in% c("uky.edu", "email.uky.edu")              ~ "UK (uky.edu)",
    recipient_domain == "gmail.com"                                  ~ "Gmail",
    recipient_domain %in% c("icloud.com", "me.com", "mac.com")       ~ "Apple iCloud",
    recipient_domain %in% c("yahoo.com", "aol.com")                  ~ "Yahoo / AOL",
    recipient_domain %in% c("outlook.com", "hotmail.com",
                            "live.com", "msn.com")                   ~ "Microsoft",
    str_detect(recipient_domain, "kyschools\\.us$|k12|isd\\.")       ~ "K-12 school",
    TRUE                                                             ~ "Other"),
    # UK first, so UK is the baseline in the categorical regression
    domain_group = factor(domain_group,
                          levels = c("UK (uky.edu)", "Gmail", "Apple iCloud",
                                     "Microsoft", "Yahoo / AOL", "K-12 school", "Other")))

# =====================================================================
# 27.2 One numerical explanatory variable
# Research question: "Do larger sends get lower open rates?"
# One row per email; only emails with 100+ recipients (no test sends).
# Send sizes run from 100 to 100,000+, so we use log10(sends):
# a 1-unit increase in log10_sends = a send 10 times larger.
# =====================================================================
status("27.2.1 EDA: open rate vs. send size (one row per email)")
emails_m9 <- ier |>
  group_by(email_name) |>
  summarize(sends     = n(),
            open_rate = mean(opened, na.rm = TRUE),
            uk_share  = mean(domain_group == "UK (uky.edu)"),
            .groups   = "drop") |>
  filter(sends >= 100) |>
  mutate(log10_sends = log10(sends))

glimpse(emails_m9)

emails_m9 |>
  slice_sample(n = 5)

emails_m9 |>
  summarize(mean_open_rate   = mean(open_rate),
            median_open_rate = median(open_rate),
            mean_sends       = mean(sends),
            median_sends     = median(sends))

emails_m9 |>
  tidy_summary(columns = c(open_rate, sends, log10_sends))

emails_m9 |>
  get_correlation(formula = open_rate ~ log10_sends)

emails_m9 |>
  summarize(correlation = cor(open_rate, log10_sends))

show(ggplot(emails_m9, aes(x = sends, y = open_rate)) +
       geom_point(alpha = 0.5) +
       labs(x = "Recipients (sends)", y = "Open rate",
            title = "Raw scale: a few huge sends squeeze everything to the left"),
     "1_scatter_raw_sends")

show(ggplot(emails_m9, aes(x = log10_sends, y = open_rate)) +
       geom_point(alpha = 0.5) +
       labs(x = "Send size (log10 of recipients: 2 = 100, 3 = 1,000, 4 = 10,000, 5 = 100,000)",
            y = "Open rate",
            title = "Relationship of send size and open rate") +
       geom_smooth(method = "lm", se = FALSE),
     "2_scatter_log_sends_regression_line")

# =====================================================================
# 27.2.2 Simple linear regression
# =====================================================================
status("27.2.2 Regression: open_rate ~ log10_sends")
size_model <- lm(open_rate ~ log10_sends, data = emails_m9)
coef(size_model)
# Slope = change in open rate when a send is 10x larger

# =====================================================================
# 27.2.3 Observed/fitted values and residuals
# =====================================================================
status("27.2.3 Fitted values and residuals per email")
size_points <- get_regression_points(size_model, ID = "email_name")
size_points

# Emails that beat / missed the prediction by the most
size_points |> arrange(desc(residual)) |> head(5)
size_points |> arrange(residual) |> head(5)

# =====================================================================
# 27.3 One categorical explanatory variable
# Does the recipient's email domain relate to whether they open?
# One row per recipient. opened is 0/1, so a group's mean = its open rate.
# =====================================================================
status("27.3.1 EDA: opens by domain group (one row per recipient)")
ier |>
  select(opened, domain_group) |>
  tidy_summary()

open_by_domain <- ier |>
  group_by(domain_group) |>
  summarize(recipients = n(),
            open_rate  = mean(opened, na.rm = TRUE),
            click_rate = mean(clicked, na.rm = TRUE))
open_by_domain

# Difference from the UK baseline (like the textbook's Table 27.7)
open_by_domain |>
  mutate(open_diff_vs_uk = open_rate - open_rate[domain_group == "UK (uky.edu)"])

show(ggplot(open_by_domain, aes(x = domain_group, y = open_rate)) +
       geom_col() +
       scale_y_continuous(labels = scales::label_percent()) +
       labs(x = "Recipient domain group", y = "Open rate",
            title = "Open rate by recipient domain group"),
     "3_bar_open_rate_by_domain")

# =====================================================================
# 27.3.2 Linear regression with a categorical explanatory variable
# =====================================================================
status("27.3.2 Regression: opened ~ domain_group")
domain_model <- lm(opened ~ domain_group, data = ier)
coef(domain_model)
# Intercept = UK open rate; each other coefficient = that group's
# difference from UK (e.g., Apple iCloud is far ABOVE, Gmail far BELOW)

click_model <- lm(clicked ~ domain_group, data = ier)
coef(click_model)

# =====================================================================
# 27.3.3 Observed/fitted values and residuals
# Every recipient in a group gets the same fitted value (the group's rate)
# =====================================================================
status("27.3.3 Fitted values by domain group")
domain_points <- get_regression_points(domain_model)
domain_points |>
  distinct(domain_group, opened_hat) |>
  arrange(desc(opened_hat))

# =====================================================================
# 27.4.1 Correlation is not necessarily causation
# Is it the PROVIDER, or the AUDIENCE? Emails sent mostly to uky.edu go
# to current students/staff; emails sent mostly to personal addresses go
# to prospective students. Audience could confound the domain effect.
# =====================================================================
status("27.4.1 Possible confounder: share of UK recipients per email")
show(ggplot(emails_m9, aes(x = uk_share, y = open_rate)) +
       geom_point(alpha = 0.5) +
       scale_x_continuous(labels = scales::label_percent()) +
       scale_y_continuous(labels = scales::label_percent()) +
       labs(x = "Share of the email's recipients at uky.edu",
            y = "Open rate",
            title = "Emails aimed at the UK community vs. open rate") +
       geom_smooth(method = "lm", se = FALSE),
     "4_scatter_uk_share_open_rate")

emails_m9 |>
  get_correlation(open_rate ~ uk_share)

emails_m9 |>
  get_correlation(log10_sends ~ uk_share)

# =====================================================================
# 27.4.2 Best-fitting line: sum of squared residuals
# =====================================================================
status("27.4.2 Sum of squared residuals")
size_points |>
  mutate(squared_residuals = residual^2) |>
  summarize(sum_of_squared_residuals = sum(squared_residuals))

# =====================================================================
# 27.4.3 Under the hood of get_regression_points()
# =====================================================================
status("27.4.3 get_regression_points() under the hood")
library(broom)
library(janitor)
size_model |>
  augment() |>
  mutate_if(is.numeric, round, digits = 3) |>
  clean_names() |>
  select(-c("std_resid", "hat", "sigma", "cooksd"))

status(paste("M9A steps ran on the IER data.", length(list.files("m9a_ier_plots")),
             "plots saved in m9a_ier_plots/."), state = "COMPLETE")
