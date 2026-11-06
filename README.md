# Research in Action

Reproducible data-cleaning and measurement-preparation pipeline for a
dynamic difference-in-differences study of entrepreneurs' well-being
before and after business failure, using longitudinal Twitter data and
LIWC-22 text scoring.

## Structure

Scripts live inside their own data folder and must be run from there
(set the working directory to the script's folder, or use Session >
Set Working Directory).

    Failed tweets/
      01_clean_failed_tweets.R        raw batches -> tweet-level cleaned data
      02_frequency_diagnostics.R      volume, word floors, coverage diagnostics
      03_build_personmonth_panel.R    person-month documents for LIWC-22,
                                      inferred failure timing
    Non-failed tweets/
      04_clean_nonfailed_tweets.R     control cleaning + overlap removal
      05_nonfailed_personmonth_docs.R seeded control subsample -> person-month
                                      documents + corpus verification

Outputs of every script are written to a `cleaned/` subfolder. The two
`liwc_manifest.csv` files (one per project) index every document in the
shared scoring folder.

## Prerequisites

R (tested on 4.5.1) with:

install.packages(c("tidyverse", "readxl"))


## Run order

1. `01` -> 2. `02` -> 3. `03` (in `Failed tweets`)
2. `04` -> `05` (in `Non-failed tweets`)

Script 05 reads the treated sample's timing file from the sibling folder
(`../Failed tweets/cleaned/`), so both folders must sit side by side
under a common parent, as in this repository.

## Data (not in this repository)

Raw and cleaned tweet data are excluded  and must be
obtained separately. Scripts expect these inputs:

- `Failed tweets/`: Batch1_before_tweets.xlsx, Batch1_after_tweets.xlsx
  , Batch2_before_tweets.csv, Batch2_after_tweets.csv
- `Non-failed tweets/`: 0_3000 (1).csv, 3001_6000 (1).csv, 6001_all (1).csv
 



LIWC-22 scoring (default dictionary) over the combined corpus of
81,854 person-month documents in `Failed tweets/cleaned/liwc_input/`,
exported as CSV keyed by filename. Scoring output is joined back via the
two manifests to build the estimation panel.

