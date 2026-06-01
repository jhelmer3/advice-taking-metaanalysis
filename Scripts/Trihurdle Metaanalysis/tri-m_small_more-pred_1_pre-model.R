
## setup ----

here::here()

library(rstan)
library(tidyverse)

options(scipen = 999)

set.seed(123)


## reading ----

dat_full <- read.csv(here::here("..", "woa_datasets.csv"))

dat <- dat_full %>%
  # removing gender because all rows that have a value for `female` also have a value for `gender`
  select(study, id, trial, firstestimate, advice, woa_winsor, female, age,
         student_judge, almanac, future, expert_advisor, incentive) |>
  # tossing trials where initial estimate was the same as the advice (gives WOA a denominator of zero)
  filter_out(firstestimate == advice) |>
  filter(between(age, 18, 100)) |>
  drop_na() |>
  mutate(distance = abs(firstestimate - advice) / firstestimate,
         woa_winsor_trim = case_when(woa_winsor == 1 ~ 0.999,
                                     woa_winsor == 0 ~ 0.001,
                                     .default = woa_winsor),
         DACM = recode_values(woa_winsor,
                              0 ~ 2,
                              1 ~ 3,
                              0.5 ~ 4,
                              default = 1)) |>
  # unfortunately tossing cases where first estimate was zero
  filter_out(distance == Inf) |>
  # giving everything a new ID (Stan needs consecutive increasing integers)
  mutate(.by = study,
         studyname = first(study),
         study = cur_group_id()) %>%
  mutate(.by = c(study, id),
         id = cur_group_id()) %>%
  mutate(.by = c(study, trial),
         trial = cur_group_id()) |>
  # standardizing distance within newly created trial (item) ID
  mutate(.by = trial,
         distance = (distance - mean(distance)) / sd(distance))

# visualization of variables
dat |>
  select(-c(study, id, trial, studyname,
            firstestimate, advice, distance)) |>
  names() |>
  map(\(var) dat |> 
        ggplot(aes(x = !!sym(var))) +
        geom_histogram(bins = 14)) |>
  patchwork::wrap_plots()

# counts of IDs
dat |>
  summarize(studies = length(unique(study)),
            people = length(unique(id)),
            items = length(unique(trial)),
            observations = nrow(dat))

# make a small version of the dataset
# currently 20 studies and 10 people within each study
dat_small <- dat |>
  filter(study %in% sample(study, 20)) %>%
  filter(.by = study,
         id %in% sample(id, 10)) %>%
  mutate(.by = study,
         study = cur_group_id()) %>%
  mutate(.by = c(study, id),
         id = cur_group_id()) %>%
  mutate(.by = c(study, trial),
         trial = cur_group_id())

saveRDS(dat_small, here::here("..", "Data Clean", "tri-m_small_more-pred_dat_2026-04-15.rds"))
