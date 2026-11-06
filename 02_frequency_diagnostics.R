# 02_frequency_diagnostics.R 
# Diagnostics on posting volume: per entrepreneur-month volume,
# retention under candidate word floors, and per-person coverage.


library(tidyverse)
library(lubridate)

failed <- read_csv("cleaned/failed_tweets_clean.csv",
                   col_types = cols(created = col_date()))

# per entrepreneur-month volume 

pm <- failed %>%
  mutate(month = floor_date(created, "month")) %>%
  group_by(entrepreneur_id, batch, month) %>%
  summarise(n_tweets = n(), n_words = sum(n_words), .groups = "drop")

write_csv(pm, "cleaned/failed_per_person_month.csv")

# retention under candidate word floors 

floors <- c(50, 100, 200, 300)
ret <- map_dfr(floors, function(w) {
  kept <- pm %>% filter(n_words >= w)
  tibble(floor_words = w,
         periods_kept  = nrow(kept),
         periods_total = nrow(pm),
         pct_periods   = round(100 * nrow(kept) / nrow(pm), 1),
         people_kept   = n_distinct(kept$entrepreneur_id),
         people_total  = n_distinct(pm$entrepreneur_id))
})
print(ret)

# distribution: words per person-month

p1 <- ggplot(pm, aes(n_words)) +
  geom_histogram(bins = 80, fill = "grey30") +
  geom_vline(xintercept = c(50, 100, 200),
             linetype = c("dotted", "dashed", "dotted"),
             colour = c("grey40", "red", "grey40")) +
  scale_x_log10(breaks = c(10, 50, 100, 200, 500, 1000, 3000)) +
  labs(x = "words per entrepreneur-month (log scale)",
       y = "number of entrepreneur-months",
       title = "Words per entrepreneur-month",
       subtitle = "Dashed red = primary 100-word floor; dotted = 50 / 200 sensitivity floors")
ggsave("cleaned/words_per_month_hist.png", p1, width = 8, height = 5)

p2 <- ggplot(pm, aes(n_words)) +
  stat_ecdf(geom = "step") +
  geom_vline(xintercept = c(50, 100, 200), linetype = "dashed",
             colour = "red") +
  scale_x_log10(breaks = c(10, 50, 100, 200, 500, 1000, 3000)) +
  scale_y_continuous(labels = function(y) paste0(100 * y, "%")) +
  labs(x = "words per entrepreneur-month (log scale)",
       y = "cumulative share of entrepreneur-months",
       title = "Cumulative distribution: floor choice and retention")
ggsave("cleaned/words_per_month_ecdf.png", p2, width = 8, height = 5)

# per-person coverage

cov <- pm %>%
  group_by(entrepreneur_id) %>%
  summarise(first_month = min(month), last_month = max(month),
            active_months = n(),
            span_months = (year(last_month) - year(first_month)) * 12 +
              month(last_month) - month(first_month) + 1,
            coverage = round(active_months / span_months, 2),
            batch = first(batch), .groups = "drop")
print(summary(cov$coverage))
write_csv(cov, "cleaned/failed_person_coverage.csv")

cov %>%
  group_by(batch) %>%
  summarise(people = n(),
            median_coverage = median(coverage),
            median_active_months = median(active_months)) %>%
  print()

