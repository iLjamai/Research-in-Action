# 01_clean_failed_tweets.R 
# Cleans the four failed-entrepreneur tweet batches into one tweet-level dataset.


library(tidyverse)
library(readxl)
library(lubridate)

in_dir  <- "."
out_dir <- "cleaned"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# bot/corporate screening:
# Excluded from the failed sample: accounts whose content provably is
# not the entrepreneur's own language. wordpressdotcom = automated
# blog-sharing service; LivingstonDaily = news outlet; pvermont and
# timbehrsin = 100% retweets (no own language); aerosuch and
# liavsagron = single retweet only. Ambiguous handles are retained.

excluded_ids <- c("wordpressdotcom", "LivingstonDaily", "pvermont",
                  "timbehrsin", "aerosuch", "liavsagron")

# Helpers

# Remove '<U+XXXX>' escape markers left by the export process.
strip_escapes <- function(x) str_replace_all(x, "<U\\+[0-9A-Fa-f]{4}>", "")

# Text cleaning: URLs, usernames, hashtags, emoji placeholders,
# HTML entities. strict_hashtag = TRUE removes the whole hashtag
# token; FALSE keeps the word text without the '#' symbol.
clean_text <- function(x, strict_hashtag = FALSE) {
  x <- str_replace_all(x, "http\\S+|www\\.\\S+", " ")    # URLs
  x <- str_replace_all(x, "@[A-Za-z0-9_]+", " ")         # usernames
  if (strict_hashtag) {
    x <- str_replace_all(x, "#[A-Za-z0-9_]+", " ")       # hashtags, strict
  } else {
    x <- str_replace_all(x, "#", "")                     # hashtags, keep words
  }
  x <- str_replace_all(x, "<ed>", " ")                   # emoji placeholders
  x <- str_replace_all(x, "&amp;", " & ")                # HTML entity unescape
  x <- strip_escapes(x)
  str_squish(x)
}

# Select the four real columns by name; export files carry junk
# index columns at the front ('...1', 'X1') and end ('...6').
standardize_cols <- function(df, file_label) {
  message(sprintf("%s: %d columns read: %s", file_label, ncol(df),
                  paste(names(df), collapse = " | ")))
  want <- c("text", "created", "screenName", "retweetCount")
  missing <- setdiff(want, names(df))
  if (length(missing) > 0)
    stop(file_label, ": expected column(s) not found: ",
         paste(missing, collapse = ", "),
         ". Available: ", paste(names(df), collapse = " | "))
  df %>% select(all_of(want))
}

# Date parsing: batch 1 stores Excel serial numbers; batch 2 uses ISO text dates. 
parse_created <- function(x, file_label) {
  x_chr <- trimws(as.character(x))
  message(sprintf("%s: created column samples: %s", file_label,
                  paste(head(unique(x_chr), 5), collapse = " ; ")))
  out <- as.POSIXct(rep(NA_real_, length(x_chr)), origin = "1970-01-01")
  is_num <- grepl("^[0-9]+(\\.[0-9]+)?$", x_chr) & x_chr != ""
  if (any(is_num)) {
    out[is_num] <- as.POSIXct(as.numeric(x_chr[is_num]) * 86400,
                              origin = "1899-12-30", tz = "UTC")
  }
  if (any(!is_num)) {
    out[!is_num] <- parse_date_time(x_chr[!is_num],
                                    orders = c("dmy", "dmy HMS", "dmy HM",
                                               "Ymd HMS", "Ymd",
                                               "mdy", "mdy HMS"),
                                    quiet = TRUE)
  }
  out <- as.Date(out)
  n_bad <- sum(is.na(out))
  if (n_bad > 0)
    message(sprintf("%s: %d of %d dates failed to parse",
                    file_label, n_bad, length(out)))
  if (!all(is.na(out))) {
    lo <- min(out, na.rm = TRUE); hi <- max(out, na.rm = TRUE)
    message(sprintf("%s: parsed range %s to %s", file_label, lo, hi))
    if (lo < as.Date("2006-01-01") || hi > Sys.Date())
      warning(file_label, ": dates outside plausible Twitter range, check!")
  }
  out
}

# readers: batch 1 (xlsx) and batch 2 (csv) 

read_b1 <- function(path, split) {
  df <- standardize_cols(read_excel(path, col_types = "text"),
                         basename(path))
  df %>%
    mutate(
      split           = split,
      batch           = "batch1",
      # person id = screenName minus the _before/_after suffix
      entrepreneur_id = str_remove(screenName, "_(before|after)$"),
      created         = parse_created(created, basename(path))
    )
}

read_b2 <- function(path, split) {
  df <- standardize_cols(
    read_csv(path, locale = locale(encoding = "Latin1"),
             col_types = cols(.default = col_character()),
             show_col_types = FALSE),
    basename(path))
  df %>%
    mutate(
      split           = split,
      batch           = "batch2",
      entrepreneur_id = screenName,
      created         = parse_created(created, basename(path))
    )
}

# cleaning pipeline 

clean_tweets <- function(df, strict_hashtag = FALSE) {
  n0 <- nrow(df)
  out <- df %>%
    filter(!is.na(entrepreneur_id)) %>%                        # no missing ids
    filter(!str_detect(text, "^\\s*RT @")) %>%                 # retweets
    mutate(text = clean_text(text, strict_hashtag)) %>%
    filter(!is.na(created), !is.na(text), nchar(text) > 0) %>%
    distinct(entrepreneur_id, text, .keep_all = TRUE) %>%      # duplicates
    transmute(
      entrepreneur_id, batch, split, created,
      retweetCount = as.numeric(retweetCount),
      text, n_words = str_count(text, "\\S+")
    )
  message(sprintf("  %d -> %d rows (-%.1f%%)", n0, nrow(out),
                  (1 - nrow(out) / n0) * 100))
  out
}

# merge failed batches

failed_raw <- bind_rows(
  read_b1(file.path(in_dir, "Batch1_before_tweets.xlsx"), "before"),
  read_b1(file.path(in_dir, "Batch1_after_tweets.xlsx"),  "after"),
  read_b2(file.path(in_dir, "Batch2_before_tweets.csv"),  "before"),
  read_b2(file.path(in_dir, "Batch2_after_tweets.csv"),   "after")
)

failed <- clean_tweets(failed_raw, strict_hashtag = FALSE)

# screening report
screen <- failed_raw %>%
  mutate(is_rt = str_detect(text, "^\\s*RT @")) %>%
  group_by(entrepreneur_id, batch) %>%
  summarise(
    n_raw        = n(),
    rt_share     = round(mean(is_rt), 2),
    mean_rtcount = round(mean(suppressWarnings(as.numeric(retweetCount)),
                              na.rm = TRUE), 1),
    .groups      = "drop"
  ) %>%
  left_join(
    failed %>%
      group_by(entrepreneur_id) %>%
      summarise(n_clean = n(), mean_words = round(mean(n_words), 1),
                .groups = "drop"),
    by = "entrepreneur_id"
  ) %>%
  arrange(desc(n_raw))

print(screen, n = Inf)

# apply exclusions

n_people_before <- n_distinct(failed$entrepreneur_id)
failed <- failed %>%
  filter(!entrepreneur_id %in% excluded_ids)
message(sprintf("screen: excluded %d IDs; %d people and %d rows remain",
                length(excluded_ids),
                n_distinct(failed$entrepreneur_id), nrow(failed)))


write_csv(failed, file.path(out_dir, "failed_tweets_clean.csv"))

# post-exclusion inspection report

failed %>% group_by(batch) %>%
  summarise(mean_words = mean(n_words), median_words = median(n_words)) %>%
  print()

failed %>%
  group_by(batch, split) %>%
  summarise(rows = n(), people = n_distinct(entrepreneur_id),
            min_date = min(created, na.rm = TRUE),
            max_date = max(created, na.rm = TRUE),
            .groups = "drop") %>%
  print()

failed %>%
  count(entrepreneur_id, name = "n_tweets") %>%
  summarise(people = n(), min = min(n_tweets), median = median(n_tweets),
            max = max(n_tweets), under_100 = sum(n_tweets < 100),
            under_300 = sum(n_tweets < 300)) %>%
  print()
