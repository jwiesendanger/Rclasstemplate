# =====================================================================
# M8A: Descriptive Statistics -- applied to MY dataset
# Individual Email Results (IER): activity_data/sample_data.csv
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

status("Loading packages and preparing the IER data")
library(tidyverse)
library(here)

emails <- read_csv(here("activity_data", "sample_data.csv"),
                   show_col_types = FALSE) |>
  rename(email_name   = `Individual Email Result: Email Name`,
         from_name    = `From Name`,
         date_sent    = `Date Sent`,
         date_bounced = `Date Bounced`,
         opened       = Opened,
         clicked      = Clicked,
         total_clicks = `Number of Total Clicks`) |>
  select(email_name, from_name, date_sent, date_bounced,
         opened, clicked, total_clicks) |>
  mutate(across(where(is.character),
                ~ ifelse(validUTF8(.x), .x, iconv(.x, "windows-1252", "UTF-8"))),
         sent_date    = as.Date(as.POSIXct(date_sent, format = "%m/%d/%Y, %I:%M %p")),
         sent_weekday = factor(weekdays(sent_date),
                               levels = c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday", "Saturday", "Sunday")),
         bounced      = !is.na(date_bounced))

# One row per email (cleaned file from M6A), limited to real sends
email_summary <- read_csv(here("activity_data", "email_summary.csv"),
                          show_col_types = FALSE)
real_sends <- email_summary |> filter(sends >= 100)

# =====================================================================
# Missing values (the chapter's NA lesson)
# =====================================================================
status("Missing values: what does NA look like in the IER data?")
emails |> summarize(across(everything(), ~ sum(is.na(.x))))
# date_bounced is NA for every email that did NOT bounce -- here NA means
# "didn't happen", not "data is missing". That's why we made bounced = !is.na().

# =====================================================================
# 24.2 summarize(): mean, sd, and friends
# =====================================================================
status("summarize(): open rate per email (100+ recipients)")
summary_open_rate <- real_sends |>
  summarize(n_emails = n(),
            mean     = mean(open_rate),
            std_dev  = sd(open_rate),
            min      = min(open_rate),
            median   = median(open_rate),
            max      = max(open_rate),
            iqr      = IQR(open_rate))
summary_open_rate

# Percentiles / quartiles of open rate
quantile(real_sends$open_rate, c(0, 0.25, 0.5, 0.75, 0.9, 1))

# Recipient-level summary: n() counts rows, sum() adds up a variable
emails |>
  summarize(recipients   = n(),
            opens        = sum(opened, na.rm = TRUE),
            clicks       = sum(clicked, na.rm = TRUE),
            bounces      = sum(bounced),
            open_rate    = mean(opened, na.rm = TRUE),
            mean_clicks_if_clicked = mean(total_clicks[clicked == 1], na.rm = TRUE))

# =====================================================================
# Close to or far from the mean? (2-sd rule) and IQR outliers
# =====================================================================
status("Which emails are unusually high or low?")
m  <- summary_open_rate$mean
s  <- summary_open_rate$std_dev
q1 <- quantile(real_sends$open_rate, 0.25)
q3 <- quantile(real_sends$open_rate, 0.75)

flagged <- real_sends |>
  mutate(sds_from_mean = round((open_rate - m) / s, 2),
         iqr_outlier   = open_rate < q1 - 1.5 * IQR(open_rate) |
                         open_rate > q3 + 1.5 * IQR(open_rate)) |>
  filter(abs(sds_from_mean) >= 2 | iqr_outlier) |>
  select(email_name, from_name, sends, open_rate, sds_from_mean, iqr_outlier) |>
  arrange(desc(open_rate))
flagged

# =====================================================================
# 24.3 group_by() + summarize()
# =====================================================================
status("group_by(): open rate by sender")
by_sender <- real_sends |>
  group_by(from_name) |>
  summarize(n_emails    = n(),
            total_sends = sum(sends),
            mean_rate   = mean(open_rate),
            sd_rate     = sd(open_rate),      # NA when a sender has 1 email
            median_rate = median(open_rate)) |>
  arrange(desc(median_rate))
by_sender

status("group_by(): by weekday (recipient level)")
by_weekday <- emails |>
  filter(!is.na(sent_weekday)) |>
  group_by(sent_weekday) |>
  summarize(recipients = n(),
            opens      = sum(opened, na.rm = TRUE),
            open_rate  = mean(opened, na.rm = TRUE),
            click_rate = mean(clicked, na.rm = TRUE))
by_weekday

# Grouping by more than one variable
status("Grouping by two variables: sender x weekday")
by_sender_weekday <- emails |>
  filter(!is.na(sent_weekday)) |>
  group_by(from_name, sent_weekday) |>
  summarize(recipients = n(),
            open_rate  = mean(opened, na.rm = TRUE))
by_sender_weekday            # still grouped by from_name (drop_last)

# Same thing, but drop all grouping explicitly
emails |>
  filter(!is.na(sent_weekday)) |>
  group_by(from_name, sent_weekday) |>
  summarize(recipients = n(), .groups = "drop")

# The chapter's mistake: a second group_by() overwrites the first
by_sender_weekday_incorrect <- emails |>
  filter(!is.na(sent_weekday)) |>
  group_by(from_name) |>
  group_by(sent_weekday) |>
  summarize(recipients = n())
by_sender_weekday_incorrect  # only 7 rows -- sender was lost

View(by_sender)
status("All M8A steps ran on the IER data. by_sender is open in the viewer.",
       state = "COMPLETE")
