# 04_clean_nonfailed_tweets.R 
# Cleans the three non-failed (control) tweet CSVs into one
# tweet-level dataset, using the identical text pipeline as the
# failed data. 

library(tidyverse)
library(lubridate)

in_dir  <- "."
out_dir <- "cleaned"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# path to the final failed sample's timing file (one row per failed
# entrepreneur); adjust to your folder structure
failed_timing_path <- "../Failed tweets/cleaned/inferred_treatment_timing.csv"

# automated/non-personal accounts excluded under the same screening
# standard as the failed sample: content is not the entrepreneur's
# own language
excluded_control_ids <- c("wordpressdotcom")

# text pipeline: identical to the failed data pipeline

strip_escapes <- function(x) str_replace_all(x, "<U\\+[0-9A-Fa-f]{4}>", "")

clean_text <- function(x) {
  x <- str_replace_all(x, "http\\S+|www\\.\\S+", " ")
  x <- str_replace_all(x, "@[A-Za-z0-9_]+", " ")
  x <- str_replace_all(x, "#", "")
  x <- str_replace_all(x, "<ed>", " ")
  x <- str_replace_all(x, "&amp;", " & ")
  x <- strip_escapes(x)
  str_squish(x)
}

standardize_cols <- function(df, file_label) {
  message(sprintf("%s: %d columns read: %s", file_label, ncol(df),
                  paste(names(df), collapse = " | ")))
  want <- c("text", "screenName", "created")
  missing <- setdiff(want, names(df))
  if (length(missing) > 0)
    stop(file_label, ": expected column(s) not found: ",
         paste(missing, collapse = ", "),
         ". Available: ", paste(names(df), collapse = " | "))
  df %>% select(all_of(want))
}

parse_created <- function(x, file_label) {
  x_chr <- trimws(as.character(x))
  message(sprintf("%s: created column samples: %s", file_label,
                  paste(head(unique(x_chr), 5), collapse = " ; ")))
  out <- parse_date_time(x_chr,
                         orders = c("Ymd", "Ymd HMS", "dmy", "dmy HMS",
                                    "mdy", "mdy HMS"),
                         quiet = TRUE)
  out <- as.Date(out)
  n_bad <- sum(is.na(out))
  if (n_bad > 0)
    message(sprintf("%s: %d of %d dates failed to parse",
                    file_label, n_bad, length(out)))
  if (!all(is.na(out))) {
    lo <- min(out, na.rm = TRUE); hi <- max(out, na.rm = TRUE)
    message(sprintf("%s: parsed range %s to %s", file_label, lo, hi))
  }
  out
}

read_control <- function(path) {
  df <- read_csv(path, col_types = cols(.default = col_character()),
                 show_col_types = FALSE)
  if (nrow(problems(df)) > 0) {
    message(sprintf("%s: %d parsing problems, retrying Latin1",
                    basename(path), nrow(problems(df))))
    df <- read_csv(path, locale = locale(encoding = "Latin1"),
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE)
  }
  df <- standardize_cols(df, basename(path))
  df %>%
    mutate(
      batch           = "control",
      entrepreneur_id = screenName,
      created         = parse_created(created, basename(path))
    )
}

clean_tweets_control <- function(df) {
  n0 <- nrow(df)
  out <- df %>%
    filter(!is.na(entrepreneur_id)) %>%                      # no missing ids
    filter(!str_detect(text, "^\\s*RT @")) %>%
    mutate(text = clean_text(text)) %>%
    filter(!is.na(created), !is.na(text), nchar(text) > 0) %>%
    distinct(entrepreneur_id, text, .keep_all = TRUE) %>%
    transmute(
      entrepreneur_id, batch, created,
      text, n_words = str_count(text, "\\S+")
    )
  message(sprintf("  %d -> %d rows (-%.1f%%)", n0, nrow(out),
                  (1 - nrow(out) / n0) * 100))
  out
}


control_files <- c("0_3000 (1).csv", "3001_6000 (1).csv", "6001_all (1).csv")

control_raw <- map_dfr(control_files,
                       ~ read_control(file.path(in_dir, .x)))

# encoding damage check (NA-safe)
control_raw %>%
  summarise(rows = n(),
            rows_with_qmark = sum(str_detect(text, "\\?"), na.rm = TRUE),
            pct_qmark = round(100 * mean(str_detect(text, "\\?"),
                                         na.rm = TRUE), 1)) %>%
  print()

control <- clean_tweets_control(control_raw)

# overlap removal: failed entrepreneurs are never controls 

failed_ids <- read_csv(failed_timing_path, col_types = cols()) %>%
  distinct(entrepreneur_id) %>%
  filter(!is.na(entrepreneur_id))

overlap_ids <- control %>%
  distinct(entrepreneur_id) %>%
  semi_join(failed_ids, by = "entrepreneur_id") %>%
  pull(entrepreneur_id)

if (length(overlap_ids) > 0) {
  control <- control %>% filter(!entrepreneur_id %in% overlap_ids)
  message(sprintf("overlap: removed %d IDs (failed entrepreneurs): %s",
                  length(overlap_ids),
                  paste(overlap_ids, collapse = ", ")))
  message(sprintf("overlap: %d people and %d rows remain",
                  n_distinct(control$entrepreneur_id), nrow(control)))
} else {
  message("overlap: none found")
}

# screen: automated/non-personal accounts 

control <- control %>% filter(!entrepreneur_id %in% excluded_control_ids)
message(sprintf("screen: removed %d automated/non-personal accounts; %d people and %d rows remain",
                length(excluded_control_ids),
                n_distinct(control$entrepreneur_id), nrow(control)))

write_csv(control, file.path(out_dir, "nonfailed_tweets_clean.csv.gz"))

# inspection report:

control %>%
  summarise(rows = n(), people = n_distinct(entrepreneur_id),
            min_date = min(created, na.rm = TRUE),
            max_date = max(created, na.rm = TRUE),
            mean_words = mean(n_words)) %>%
  print()

control %>%
  count(entrepreneur_id, name = "n_tweets") %>%
  summarise(people = n(), min = min(n_tweets), median = median(n_tweets),
            max = max(n_tweets)) %>%
  print()
