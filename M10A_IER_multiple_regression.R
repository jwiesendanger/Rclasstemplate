# =====================================================================
# M10A: Multiple Regression -- applied to OUR IER sample data
# Data: activity_data/ier_deidentified.csv.gz (de-identified IER export)
#
# Mirrors the textbook steps, one row per email (100+ recipients):
#   30.1  one numerical + one categorical explanatory variable
#         open_rate ~ log10_sends (x) and audience (Mostly UK vs. Mostly external)
#   30.2  two numerical explanatory variables
#         open_rate ~ log10_sends + gmail_share
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# Plots show in the Plots pane and are saved to m10a_ier_plots/.
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

dir.create("m10a_ier_plots", showWarnings = FALSE)
show <- function(p, name) {
  print(p)
  ggplot2::ggsave(file.path("m10a_ier_plots", paste0(name, ".png")), p,
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
  rename(email_name = `Individual Email Result: Email Name`,
         opened     = Opened,
         clicked    = Clicked)

# One row per email, with the mix of recipient domains
emails_m10 <- ier |>
  group_by(email_name) |>
  summarize(sends        = n(),
            open_rate    = mean(opened, na.rm = TRUE),
            uk_share     = mean(recipient_domain %in% c("uky.edu", "email.uky.edu")),
            gmail_share  = mean(recipient_domain == "gmail.com"),
            apple_share  = mean(recipient_domain %in% c("icloud.com", "me.com", "mac.com")),
            .groups = "drop") |>
  filter(sends >= 100) |>
  mutate(log10_sends = log10(sends),
         audience    = factor(if_else(uk_share >= 0.5, "Mostly UK", "Mostly external"),
                              levels = c("Mostly UK", "Mostly external")))

# =====================================================================
# 30.1 One numerical and one categorical explanatory variable
# 30.1.1 Exploratory data analysis
# =====================================================================
status("30.1.1 EDA: open rate, send size, audience")
glimpse(emails_m10)

emails_m10 |> sample_n(size = 10)

emails_m10 |>
  select(open_rate, log10_sends, audience) |>
  tidy_summary()

emails_m10 |>
  get_correlation(formula = open_rate ~ log10_sends)

show(ggplot(emails_m10, aes(x = log10_sends, y = open_rate, color = audience)) +
       geom_point() +
       labs(x = "Send size (log10 of recipients: 2 = 100, 3 = 1,000, 4 = 10,000)",
            y = "Open rate", color = "Audience") +
       geom_smooth(method = "lm", se = FALSE),
     "1_interaction_lines")

# =====================================================================
# 30.1.2 Model with interactions
# =====================================================================
status("30.1.2 One-factor model, then interaction model")
one_factor_model <- lm(open_rate ~ audience, data = emails_m10)
coef(one_factor_model)     # intercept = Mostly UK average; offset = Mostly external

model_int <- lm(open_rate ~ log10_sends * audience, data = emails_m10)
coef(model_int)

# Intercept and slope for each audience
b <- coef(model_int)
tibble(audience  = levels(emails_m10$audience),
       intercept = b["(Intercept)"] + c(0, b["audienceMostly external"]),
       slope     = b["log10_sends"] + c(0, b["log10_sends:audienceMostly external"]))

# =====================================================================
# 30.1.3 Model without interactions (parallel slopes)
# =====================================================================
status("30.1.3 Parallel slopes model")
show(ggplot(emails_m10, aes(x = log10_sends, y = open_rate, color = audience)) +
       geom_point() +
       labs(x = "Send size (log10 of recipients)", y = "Open rate", color = "Audience") +
       geom_parallel_slopes(se = FALSE),
     "2_parallel_slopes")

model_no_int <- lm(open_rate ~ log10_sends + audience, data = emails_m10)
coef(model_no_int)

# =====================================================================
# 30.1.4 Observed responses, fitted values, and residuals
# =====================================================================
status("30.1.4 Fitted values and residuals (interaction model)")
regression_points <- get_regression_points(model_int, ID = "email_name")
regression_points

# =====================================================================
# 30.2 Two numerical explanatory variables
# 30.2.1 Exploratory data analysis
# Question: are big sends less opened because they go to more Gmail
# addresses (where opens are low), or is size its own effect?
# =====================================================================
status("30.2.1 EDA: open rate, send size, Gmail share")
emails_m10 |>
  select(open_rate, log10_sends, gmail_share) |>
  tidy_summary()

show(ggplot(emails_m10, aes(x = log10_sends, y = open_rate)) +
       geom_point() +
       labs(x = "Send size (log10 of recipients)", y = "Open rate",
            title = "Open rate and send size") +
       geom_smooth(method = "lm", se = FALSE),
     "3a_open_vs_size")

show(ggplot(emails_m10, aes(x = gmail_share, y = open_rate)) +
       geom_point() +
       scale_x_continuous(labels = scales::label_percent()) +
       labs(x = "Share of recipients on Gmail", y = "Open rate",
            title = "Open rate and Gmail share") +
       geom_smooth(method = "lm", se = FALSE),
     "3b_open_vs_gmail_share")

emails_m10 |> get_correlation(open_rate ~ log10_sends)
emails_m10 |> get_correlation(open_rate ~ gmail_share)

# Correlation matrix
emails_m10 |> select(open_rate, log10_sends, gmail_share, apple_share) |> cor()

# =====================================================================
# 30.2.2 Multiple regression with two numerical regressors
# =====================================================================
status("30.2.2 Regression plane: open_rate ~ log10_sends + gmail_share")
open_model <- lm(open_rate ~ log10_sends + gmail_share, data = emails_m10)
coef(open_model)

# Partial vs. simple slope for send size
simple_model <- lm(open_rate ~ log10_sends, data = emails_m10)
coef(simple_model)

# =====================================================================
# 30.2.3 Observed/fitted values and residuals
# =====================================================================
status("30.2.3 Fitted values and residuals (regression plane)")
get_regression_points(open_model, ID = "email_name")

status(paste("M10A steps ran on the IER data.", length(list.files("m10a_ier_plots")),
             "plots saved in m10a_ier_plots/."), state = "COMPLETE")
