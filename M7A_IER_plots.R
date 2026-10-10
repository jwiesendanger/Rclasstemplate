# =====================================================================
# M7A: Data Visualization -- applied to MY dataset
# Individual Email Results (IER): activity_data/sample_data.csv
# Uses the five named graphs (5NG) from the chapter, each matched to
# the question it answers best.
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# Every plot is shown in the Plots pane and saved to my_data_plots/.
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

status("Loading packages")
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)

dir.create(here("my_data_plots"), showWarnings = FALSE)
show <- function(p, name) {
  print(p)
  ggsave(here("my_data_plots", paste0(name, ".png")), p,
         width = 9, height = 5.5, dpi = 100, bg = "white")
  invisible(p)
}

# One accent color for single-series plots; a clean theme for all
accent <- "steelblue"
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold")))
pct <- scales::label_percent(accuracy = 1)

# ---------------------------------------------------------------------
# Data prep (same cleaning as M6A_my_data_wrangling.R)
# ---------------------------------------------------------------------
status("Preparing the IER data")
emails <- read_csv(here("activity_data", "sample_data.csv"),
                   show_col_types = FALSE) |>
  rename(email_name   = `Individual Email Result: Email Name`,
         from_name    = `From Name`,
         date_sent    = `Date Sent`,
         date_bounced = `Date Bounced`,
         opened       = Opened,
         clicked      = Clicked) |>
  select(email_name, from_name, date_sent, date_bounced, opened, clicked) |>
  mutate(across(where(is.character),
                ~ ifelse(validUTF8(.x), .x, iconv(.x, "windows-1252", "UTF-8"))),
         sent_datetime = as.POSIXct(date_sent, format = "%m/%d/%Y, %I:%M %p"),
         sent_date     = as.Date(sent_datetime),
         sent_weekday  = factor(weekdays(sent_date),
                                levels = c("Monday", "Tuesday", "Wednesday",
                                           "Thursday", "Friday", "Saturday", "Sunday")))

# The cleaned per-email file from M6A (one row per email)
email_summary <- read_csv(here("activity_data", "email_summary.csv"),
                          show_col_types = FALSE)

# Very small sends (tests, single-recipient emails) have open rates of
# exactly 0% or 100% and distort the picture. The scatterplot below shows
# them; the histogram and boxplot use only emails with 100+ recipients.
real_sends <- email_summary |> filter(sends >= 100)
c(all_emails = nrow(email_summary), with_100_plus_sends = nrow(real_sends))

# =====================================================================
# 5NG#3 HISTOGRAM -- How are open rates distributed across emails?
# (distribution of ONE numerical variable)
# =====================================================================
status("Histogram: distribution of open rates")
show(ggplot(real_sends, aes(x = open_rate)) +
       geom_histogram(binwidth = 0.05, boundary = 0,
                      color = "white", fill = accent) +
       scale_x_continuous(labels = pct) +
       labs(title = "How open rates are distributed across emails",
            subtitle = "Emails with 100+ recipients",
            x = "Open rate (per email)", y = "Number of emails"),
     "1_hist_open_rate")

# =====================================================================
# 5NG#1 SCATTERPLOT -- Do bigger sends get lower open rates?
# (relationship between TWO numerical variables)
# log10 x-axis because send sizes range from a handful to 100k+;
# alpha handles overplotting, as in the chapter
# =====================================================================
status("Scatterplot: send size vs open rate")
show(ggplot(email_summary, aes(x = sends, y = open_rate)) +
       geom_point(alpha = 0.5, size = 2.5, color = accent) +
       scale_x_log10(labels = scales::label_comma()) +
       scale_y_continuous(labels = pct) +
       geom_vline(xintercept = 100, linetype = "dashed", color = "grey50") +
       labs(title = "Open rate vs. send size",
            subtitle = "Each point is one email (log scale). Left of the dashed line = tiny/test sends at 0% or 100%",
            x = "Recipients (sends)", y = "Open rate"),
     "2_scatter_sends_vs_open_rate")

# =====================================================================
# 5NG#2 LINEGRAPH -- How did volume change day by day?
# (numerical variable over TIME)
# Sends and opens are on very different scales, so they get separate
# facets with their own y-axis instead of one crowded chart
# =====================================================================
status("Linegraph: daily sends and opens")
# Nearly all volume is in the most recent month, so zoom to the last 30 days
last_day <- max(emails$sent_date, na.rm = TRUE)
daily_tidy <- emails |>
  filter(!is.na(sent_date), sent_date > last_day - 30) |>
  group_by(sent_date) |>
  summarize(Sent = n(), Opened = sum(opened, na.rm = TRUE)) |>
  pivot_longer(cols = -sent_date, names_to = "metric", values_to = "count") |>
  mutate(metric = factor(metric, levels = c("Sent", "Opened")))

show(ggplot(daily_tidy, aes(x = sent_date, y = count)) +
       geom_line(linewidth = 0.8, color = accent) +
       geom_point(size = 1.5, color = accent) +
       facet_wrap(~ metric, ncol = 1, scales = "free_y") +
       scale_y_continuous(labels = scales::label_comma()) +
       labs(title = "Daily email volume (last 30 days of data)",
            x = "Date sent", y = "Emails"),
     "3_line_daily_volume")

# =====================================================================
# 5NG#5 BARPLOT -- Which weekday gets the best open rate?
# (categorical x; rate is PRE-COUNTED -> geom_col)
# =====================================================================
status("Barplot: open rate by weekday")
weekday_rates <- emails |>
  filter(!is.na(sent_weekday)) |>
  group_by(sent_weekday) |>
  summarize(sends = n(), open_rate = mean(opened, na.rm = TRUE))
weekday_rates

show(ggplot(weekday_rates, aes(x = sent_weekday, y = open_rate)) +
       geom_col(fill = accent, width = 0.7) +
       geom_text(aes(label = pct(open_rate)), vjust = -0.4, size = 3.5) +
       scale_y_continuous(labels = pct, expand = expansion(mult = c(0, 0.1))) +
       labs(title = "Open rate by day the email was sent",
            x = NULL, y = "Open rate"),
     "4_bar_weekday_open_rate")

# Barplot NOT pre-counted -> geom_bar(): how many sends per weekday
show(ggplot(filter(emails, !is.na(sent_weekday)), aes(x = sent_weekday)) +
       geom_bar(fill = accent, width = 0.7) +
       scale_y_continuous(labels = scales::label_comma(),
                          expand = expansion(mult = c(0, 0.05))) +
       labs(title = "Number of sends by weekday", x = NULL, y = "Sends"),
     "5_bar_weekday_sends")

# =====================================================================
# 5NG#4 BOXPLOT -- Which senders' emails perform best?
# (numerical variable split by a CATEGORICAL variable)
# Only senders with 3+ emails, so each box has something to summarize.
# Horizontal (y = sender) so the long names are readable.
# =====================================================================
status("Boxplot: open rate by sender")
sender_counts <- email_summary |> count(from_name, name = "n_emails")
sender_counts |> arrange(desc(n_emails))

box_data <- real_sends |>
  inner_join(count(real_sends, from_name, name = "n_emails"), by = "from_name") |>
  filter(n_emails >= 3) |>
  mutate(from_name = reorder(from_name, open_rate, FUN = median))

show(ggplot(box_data, aes(x = open_rate, y = from_name)) +
       geom_boxplot(fill = "grey92", outlier.shape = NA) +
       geom_jitter(height = 0.15, width = 0, alpha = 0.6,
                   size = 2, color = accent) +
       scale_x_continuous(labels = pct) +
       labs(title = "Open rate by sender (senders with 3+ emails of 100+ recipients)",
            subtitle = "Box = middle 50% of that sender's emails; dots = individual emails",
            x = "Open rate", y = NULL),
     "6_box_open_rate_by_sender")

status(paste(length(list.files(here("my_data_plots"))),
             "plots saved in my_data_plots/ -- page through them with the Plots pane arrows."),
       state = "COMPLETE")
