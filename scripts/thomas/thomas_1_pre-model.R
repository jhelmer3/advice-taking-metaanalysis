
## setup ----

here::here()

library(tidyverse)

options(scipen = 999)

## reading ----

dat_full <- read.csv(here::here("..", "thomas_data.csv"))

dat <- dat_full |>
  janitor::clean_names() |>
  # just doing a couple predictors for now
  select(id, trial, firstestimate = initial_estimate, advice, woa_winsor = at_winsorized, 
         gender, age, starts_with("big5_"), npi_narcissism, anxiety) |>
  # tossing trials where initial estimate was the same as the advice (gives WOA a denominator of zero)
  filter_out(firstestimate == advice) |>
  filter(between(age, 18, 100)) |>
  drop_na() |>
  mutate(female = recode_values(gender, # I HAVE NO IDEA HOW GENDER IS CODED
                                2 ~ 1,
                                1 ~ 0,
                                3 ~ 2),
         distance = abs(firstestimate - advice) / firstestimate,
         woa_winsor_trim = case_when(woa_winsor == 1 ~ 0.999,
                                     woa_winsor == 0 ~ 0.001,
                                     .default = woa_winsor),
         DACM = recode_values(woa_winsor,
                              0 ~ 2,
                              1 ~ 3,
                              0.5 ~ 4,
                              default = 1)) |>
  select(-gender) |>
  # unfortunately tossing cases where first estimate was zero
  filter_out(distance == Inf) |>
  # giving everything a new ID (Stan needs consecutive increasing integers)
  mutate(.by = c(id),
         id = cur_group_id()) |>
  mutate(.by = c(trial),
         trial = cur_group_id()) |>
  # standardizing distance within newly created trial (item) ID
  mutate(.by = trial,
         distance = (distance - mean(distance)) / sd(distance))

# visualization of variables
dat |>
  select(-c(id, trial,
            firstestimate, advice, distance)) |>
  names() |>
  map(\(var) dat |> 
        ggplot(aes(x = !!sym(var))) +
        geom_histogram(bins = 14) +
        theme_classic()) |>
  patchwork::wrap_plots()

# counts of IDs
dat |>
  summarize(people = length(unique(id)),
            items = length(unique(trial)),
            observations = nrow(dat))

# make a small version of the dataset
# 50 people
dat_small <- dat |>
  filter(id %in% sample(id, 25)) %>%
  mutate(.by = c(id),
         id = cur_group_id()) %>%
  mutate(trial = cur_group_id())

saveRDS(dat_small, here::here("..", "Data Clean", "Thomas", "thomas_small_dat_2026-04-20.rds"))
saveRDS(dat, here::here("..", "Data Clean", "Thomas", "thomas_dat_2026-05-21.rds"))
