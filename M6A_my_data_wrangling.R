# =====================================================================
# M6A: Wrangling and Tidying Data -- applied to MY dataset
# Dataset: activity_data/sample_data.csv (email send/engagement results)
# Follows the steps in:
# https://spencergreenhalgh.com/ict_lis_661_textbook_2025_fall/_book/m6a-wrangling-and-tidying-data
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first so here() finds the data.
# Then run line by line (Ctrl+Enter) or all at once (Ctrl+Shift+S).
# =====================================================================

# --- Status banner (so you can tell what's happening in the console) --
status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

status("Step 0/9: loading packages")
library(dplyr)
library(readr)
library(tidyr)
library(here)

# =====================================================================
# 18.3.1 Importing data
# =====================================================================
status("Step 1/9: importing sample_data.csv")
emails_raw <- read_csv(here("activity_data", "sample_data.csv"),
                       show_col_types = FALSE)
glimpse(emails_raw)

# =====================================================================
# 18.2.7 rename() + select()  -- clean up the column names first
# Column names with spaces need backticks, like `Contact ID`.
# The new name goes BEFORE the = sign.
# =====================================================================
status("Step 2/9: rename() and select() columns")
emails <- emails_raw |>
  rename(email_name        = `Individual Email Result: Email Name`,
         contact_id        = `Contact ID`,
         batch_id          = BatchID,
         clicked           = Clicked,
         date_bounced      = `Date Bounced`,
         date_sent         = `Date Sent`,
         date_unsubscribed = `Date Unsubscribed`,
         email_asset_id    = `Email Asset ID`,
         email_id          = `Email ID`,
         from_address      = `From Address`,
         dm_tracking_id    = `DM Tracking ID`,
         email             = Email,
         from_name         = `From Name`,
         hard_bounce       = `Hard Bounce`,
         links_clicked     = `Links Clicked`,
         total_clicks      = `Number of Total Clicks`,
         opened            = Opened,
         subscriber_id     = SubscriberID) |>
  # de-select (drop) columns we don't need, using the - sign
  select(-email, -dm_tracking_id, -email_asset_id) |>
  # one email name was cut off mid-character in the export; strip the
  # broken byte so View() doesn't error with "invalid multibyte string"
  mutate(across(where(is.character), ~ iconv(.x, "UTF-8", "UTF-8", sub = "")))

glimpse(emails)

# select() helpers, like the textbook's starts_with()/ends_with()/contains()
emails |> select(starts_with("date")) |> head()
emails |> select(contains("click")) |> head()

# relocate() the key columns to the front
emails <- emails |>
  relocate(email_name, from_name, date_sent, .before = everything())

# =====================================================================
# 18.2.3 mutate() -- create new variables from existing ones
# =====================================================================
status("Step 3/9: mutate() new variables")
emails <- emails |>
  mutate(
    sent_datetime = as.POSIXct(date_sent, format = "%m/%d/%Y, %I:%M %p"),
    sent_date     = as.Date(sent_datetime),
    sent_weekday  = weekdays(sent_date),
    sent_hour     = as.integer(format(sent_datetime, "%H")),
    bounced       = !is.na(date_bounced),
    unsubscribed  = !is.na(date_unsubscribed)
  )

emails |> select(date_sent, sent_datetime, sent_weekday, bounced, unsubscribed) |> head()

# =====================================================================
# 18.2.2 filter() rows
# == equal, != not equal, >, <, >=, <=, & and, | or, ! not, %in%
# =====================================================================
status("Step 4/9: filter() rows")
opened_emails  <- emails |> filter(opened == 1)
clicked_emails <- emails |> filter(opened == 1 & clicked == 1)
bounced_emails <- emails |> filter(bounced)
not_bounced    <- emails |> filter(!bounced)
weekend_sends  <- emails |> filter(sent_weekday %in% c("Saturday", "Sunday"))

c(all        = nrow(emails),
  opened     = nrow(opened_emails),
  clicked    = nrow(clicked_emails),
  bounced    = nrow(bounced_emails),
  not_bounced = nrow(not_bounced),
  weekend    = nrow(weekend_sends))

# =====================================================================
# 18.2.4 group_by() + summarize() + arrange()
# =====================================================================
status("Step 5/9: group_by(), summarize(), arrange()")
email_summary <- emails |>
  group_by(email_name, from_name) |>
  summarize(sends        = n(),
            opens        = sum(opened, na.rm = TRUE),
            clicks       = sum(clicked, na.rm = TRUE),
            bounces      = sum(bounced),
            unsubscribes = sum(unsubscribed),
            .groups = "drop") |>
  mutate(open_rate  = round(opens  / sends, 3),
         click_rate = round(clicks / sends, 3))

email_summary |> arrange(sends)          # ascending (default)
email_summary |> arrange(desc(sends))    # descending

# =====================================================================
# 18.2.5 join data frames
# Build a sender-level table, then join it back to the email table.
# =====================================================================
status("Step 6/9: inner_join() data frames")
sender_summary <- emails |>
  group_by(from_name) |>
  summarize(sender_emails = n_distinct(email_name),
            sender_sends  = n(),
            sender_open_rate = round(mean(opened, na.rm = TRUE), 3))

# Matching key names: both tables have from_name
emails_with_sender <- email_summary |>
  inner_join(sender_summary, by = "from_name")

# Different key names: rename the key in one table, then use c("a" = "b")
sender_lookup <- sender_summary |> rename(sender = from_name)
emails_with_sender2 <- email_summary |>
  inner_join(sender_lookup, by = c("from_name" = "sender"))

# Multiple keys: email + day it was sent
daily_by_email <- emails |>
  group_by(email_name, sent_date) |>
  summarize(daily_sends = n(), .groups = "drop")
email_day_joined <- emails |>
  inner_join(daily_by_email, by = c("email_name", "sent_date"))

glimpse(emails_with_sender)

# =====================================================================
# 18.2.8 top_n() values of a variable
# =====================================================================
status("Step 7/9: top_n()")
top_open_rates <- email_summary |>
  filter(sends >= 1000) |>                 # only emails with real volume
  top_n(n = 10, wt = open_rate) |>
  arrange(desc(open_rate))
top_open_rates

# =====================================================================
# 18.3.4 Converting to "tidy" data with pivot_longer()
# email_summary is WIDE: opens / clicks / bounces / unsubscribes are
# all the same kind of thing (a count of an outcome) spread across columns.
# =====================================================================
status("Step 8/9: pivot_longer() to tidy format")
outcomes_wide <- email_summary |>
  select(email_name, opens, clicks, bounces, unsubscribes)
outcomes_wide

outcomes_tidy <- outcomes_wide |>
  pivot_longer(names_to  = "outcome",
               values_to = "count",
               cols      = -email_name)
outcomes_tidy

# Same result, naming the columns to tidy instead of the one to skip
outcomes_wide |>
  pivot_longer(names_to = "outcome", values_to = "count",
               cols = opens:unsubscribes)

# Daily totals: wide -> tidy (like the Guatemala example)
daily_wide <- emails |>
  group_by(sent_date) |>
  summarize(sent = n(), opened = sum(opened, na.rm = TRUE),
            clicked = sum(clicked, na.rm = TRUE))
daily_tidy <- daily_wide |>
  pivot_longer(names_to = "metric", values_to = "count", cols = -sent_date)
daily_tidy

# =====================================================================
# Done -- open results in the spreadsheet viewer
# =====================================================================
status("Step 9/9: opening results in the viewer")
View(email_summary)
View(top_open_rates)
View(outcomes_tidy)

status("All M6A steps ran on sample_data.csv. Results are in the viewer tabs.",
       state = "COMPLETE")
