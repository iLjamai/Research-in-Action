# 03_build_personmonth_panel.R 
# Aggregates cleaned tweets into one document per entrepreneur per
# calendar month and exports them for LIWC-22 scoring.
# Infers per-entrepreneur treatment timing from the before/after
# split boundaries.


library(tidyverse)
library(lubridate)

failed <- read_csv("cleaned/failed_tweets_clean.csv",
                   col_types = cols(created = col_date()))

# person-month documents (.txt)

docs <- failed %>%
  mutate(month = floor_date(created, "month")) %>%
  group_by(entrepreneur_id, batch, month) %>%
  summarise(
    n_tweets = n(),
    n_words  = sum(n_words),
    # join with newlines: keeps tweet boundaries as sentence breaks
    text_doc = paste(text, collapse = "\n"),
    .groups  = "drop"
  ) %>%
  arrange(entrepreneur_id, month) %>%
  mutate(
    file_id   = str_replace_all(entrepreneur_id, "[^A-Za-z0-9_.-]", "_"),
    file_name = sprintf("%s__%s.txt", file_id, format(month, "%Y-%m")),
    floor50   = n_words >=  50,
    floor100  = n_words >= 100,
    floor200  = n_words >= 200
  )

# retention by batch
docs %>%
  group_by(batch) %>%
  summarise(people = n_distinct(entrepreneur_id),
            person_months = n(),
            keep50 = sum(floor50), keep100 = sum(floor100),
            keep200 = sum(floor200)) %>%
  print()

# export one .txt per person-month; floor50 is the export floor so that
# all documents any floor variant might need are scored exactly once
dir.create("cleaned/liwc_input", showWarnings = FALSE)
docs_kept <- docs %>% filter(floor50)
walk2(docs_kept$text_doc, docs_kept$file_name,
      ~ write_lines(.x, file.path("cleaned/liwc_input", .y)))

# manifest: joins LIWC scores (keyed by file_name) back to person-months
docs_kept %>%
  transmute(file_name, entrepreneur_id, batch, month,
            n_tweets, n_words, floor100, floor200) %>%
  write_csv("cleaned/liwc_manifest.csv")

# inferred treatment timing:
# The failure date lies between each person's last before-tweet and
# first after-tweet. gname_mid = midpoint of that interval (primary);
# gname_earliest / gname_latest = bounding sensitivity assignments.
timing <- failed %>%
  group_by(entrepreneur_id, split) %>%
  summarise(
    boundary = if (first(split) == "before") max(created) else min(created),
    .groups  = "drop"
  ) %>%
  pivot_wider(names_from = split, values_from = boundary) %>%
  rename(last_before = before, first_after = after)

timing <- timing %>%
  mutate(
    gap_days        = as.numeric(first_after - last_before),
    gname_mid       = floor_date(last_before + (first_after - last_before) / 2,
                                 "month"),
    gname_earliest  = floor_date(last_before, "month") %m+% months(1),
    gname_latest    = floor_date(first_after, "month"),
    # exact count of calendar months covered by the interval
    interval_months = (year(first_after) - year(last_before)) * 12 +
      month(first_after) - month(last_before) + 1
  )

# people appearing in only one split cannot be assigned a timing (NA)
message(sprintf("people with only one side (NA gap): %d",
                sum(is.na(timing$gap_days))))
message("gap (days) between last before- and first after-tweet:")
print(summary(timing$gap_days))
message("share of gaps <= 31 days:")
print(mean(timing$gap_days <= 31, na.rm = TRUE))
message("share of gaps <= 92 days:")
print(mean(timing$gap_days <= 92, na.rm = TRUE))

write_csv(timing, "cleaned/inferred_treatment_timing.csv")
