# =====================================================================
# Project 2 data prep: make a DE-IDENTIFIED copy of the IER export
# Input : activity_data/sample_data.csv   (raw -- keep OFF GitHub)
# Output: activity_data/ier_deidentified.csv.gz  (safe to commit)
#
# Removes: Email, Contact ID, SubscriberID, DM Tracking ID
# Adds   : recipient_domain (the part of the email after the @)
# Changes: From Address -> sender_domain; any From Name that contains an
#          email address is redacted to "[redacted]@domain"
# =====================================================================
library(tidyverse)
library(here)

raw <- read_csv(here("activity_data", "sample_data.csv"), show_col_types = FALSE)

ier_deid <- raw |>
  mutate(recipient_domain = tolower(sub(".*@", "", Email)),
         sender_domain    = tolower(sub(".*@", "", `From Address`)),
         `From Name`      = sub("[^[:space:]<>\"']+@", "[redacted]@", `From Name`,
                                useBytes = TRUE)) |>
  select(-Email, -`Contact ID`, -SubscriberID, -`DM Tracking ID`, -`From Address`) |>
  mutate(across(where(is.character),
                ~ ifelse(validUTF8(.x), .x, iconv(.x, "windows-1252", "UTF-8"))))

# Sanity checks: no "@" left anywhere, same number of rows
stopifnot(nrow(ier_deid) == nrow(raw))
# the only "@" allowed is in a redacted From Name
stopifnot(!any(sapply(select(ier_deid, -`From Name`),
                      function(col) any(grepl("@", col, fixed = TRUE, useBytes = TRUE)))))
stopifnot(all(!grepl("@", ier_deid$`From Name`, fixed = TRUE, useBytes = TRUE) |
                grepl("[redacted]@", ier_deid$`From Name`, fixed = TRUE, useBytes = TRUE)))

write_csv(ier_deid, here("activity_data", "ier_deidentified.csv.gz"))

cat("Rows:", nrow(ier_deid), " Columns:", ncol(ier_deid), "\n")
cat("File size (MB):",
    round(file.size(here("activity_data", "ier_deidentified.csv.gz")) / 1e6, 1), "\n")
ier_deid |> count(recipient_domain, sort = TRUE) |> head(15) |> print()
