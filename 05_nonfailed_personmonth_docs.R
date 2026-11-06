# 05_nonfailed_personmonth_docs.R 
# Draws a seeded random control subsample, aggregates cleaned control
# tweets into one document per person per calendar month, and exports
# the documents for LIWC-22 scoring.


library(tidyverse)
library(lubridate)

# always adjust working paths to your own folder structure
failed_dir  <- "."                                         # <- Failed tweets
control_dir <- "../Non-failed tweets"                      # <- Non-failed tweets
liwc_dir    <- file.path(failed_dir, "cleaned/liwc_input")
out_dir     <- file.path(control_dir, "cleaned")

# control subsample

set.seed(42)
control <- read_csv(file.path(control_dir, "cleaned/nonfailed_tweets_clean.csv.gz"),
                    col_types = cols(created = col_date()))

subsample_ids <- control %>%
  distinct(entrepreneur_id) %>%
  slice_sample(n = 2000) %>%
  pull(entrepreneur_id)

# second independent draw for the subsample robustness check
# set.seed(43); subsample_ids_43 <- control %>%
#   distinct(entrepreneur_id) %>% slice_sample(n = 2000) %>% pull()

control_sub <- control %>%
  filter(entrepreneur_id %in% subsample_ids)

message(sprintf("subsample: %d control entrepreneurs drawn",
                n_distinct(control_sub$entrepreneur_id)))

# person-month documents (.txt)

docs_c <- control_sub %>%
  mutate(month = floor_date(created, "month")) %>%
  group_by(entrepreneur_id, month) %>%
  summarise(
    n_tweets = n(),
    n_words  = sum(n_words),
    text_doc = paste(text, collapse = "\n"),
    .groups  = "drop"
  ) %>%
  arrange(entrepreneur_id, month) %>%
  mutate(
    file_id   = str_replace_all(entrepreneur_id, "[^A-Za-z0-9_.-]", "_"),
    # 'ctl__' prefix keeps control filenames distinct from failed ones
    file_name = sprintf("ctl__%s__%s.txt", file_id, format(month, "%Y-%m")),
    floor50   = n_words >=  50,
    floor100  = n_words >= 100,
    floor200  = n_words >= 200
  )

# retention
docs_c %>%
  summarise(people = n_distinct(entrepreneur_id),
            person_months = n(),
            keep50 = sum(floor50), keep100 = sum(floor100),
            keep200 = sum(floor200)) %>%
  print()

# export and manifest

docs_kept_c <- docs_c %>% filter(floor50)
walk2(docs_kept_c$text_doc, docs_kept_c$file_name,
      ~ write_lines(.x, file.path(liwc_dir, .y)))

# manifest: joins LIWC scores back to person-months
docs_kept_c %>%
  transmute(file_name, entrepreneur_id, month,
            n_tweets, n_words, floor100, floor200) %>%
  write_csv(file.path(out_dir, "nonfailed_liwc_manifest.csv"))

message(sprintf("exported %d control documents to %s",
                nrow(docs_kept_c), liwc_dir))

# inspection report 

docs_c %>%
  group_by(entrepreneur_id) %>%
  summarise(active_months = n(), .groups = "drop") %>%
  summarise(people = n(), median_active_months = median(active_months),
            p25 = quantile(active_months, .25),
            p75 = quantile(active_months, .75)) %>%
  print()


# verify scoring corpus:
# The scoring folder must contain exactly the union of both manifests:
# no stale files, no missing files.

failed_manifest <- read_csv(file.path(failed_dir, "cleaned/liwc_manifest.csv"),
                            col_types = cols())
control_manifest <- read_csv(file.path(out_dir, "nonfailed_liwc_manifest.csv"),
                             col_types = cols())

docs_expected <- c(failed_manifest$file_name, control_manifest$file_name)
files_present <- list.files(liwc_dir, pattern = "\\.txt$")

message(sprintf("expected: %d | present: %d | stray: %d | missing: %d",
                length(docs_expected), length(files_present),
                length(setdiff(files_present, docs_expected)),
                length(setdiff(docs_expected, files_present))))

# remove any stale files not in either manifest
stray <- setdiff(files_present, docs_expected)
if (length(stray) > 0) {
  file.remove(file.path(liwc_dir, stray))
  message(sprintf("removed %d stale documents", length(stray)))
}

