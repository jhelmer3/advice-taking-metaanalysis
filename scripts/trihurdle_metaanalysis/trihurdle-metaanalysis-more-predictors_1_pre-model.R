
## setup ----

here::here()

library(rstan)
library(bridgesampling)
library(loo)
library(bayesplot)
library(bayestestR)
library(tidyverse)

options(scipen = 999)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)

set.seed(123)


## reading ----

dat_full <- read.csv(here::here("..", "woa_datasets.csv"))

studynames <- dat_full %>%
  select(!c(gender, published, base, female_percentage,
            mean_age, student_judge, almanac, future,
            incentive, abs_value, winsor)) %>%
  mutate(distance = abs(firstestimate - advice) / firstestimate) %>%
  filter(!is.na(woa_raw) & !is.na(zconfidence) & distance < 1) %>%
  select(study) %>% 
  summarize(.by = study,
            studyname = first(study),
            study = as.character(cur_group_id()))

dat <- dat_full %>%
  # removing gender because all rows that have a value for `female` also have a value for `gender`
  select(study, id, trial, firstestimate, advice, woa_winsor, zconfidence, female, age,
         student_judge, almanac, future, expert_advisor,
         incentive) |>
  mutate(distance = abs(firstestimate - advice) / firstestimate,
         .keep = "unused") %>%
  filter(!is.na(woa_winsor) & !is.na(zconfidence) & !is.na(age) & !is.na(female) & distance < 1) %>%
  filter(age > 18) |>
  mutate(.by = study,
         studyname = first(study),
         study = cur_group_id()) %>%
  mutate(.by = c(study, id),
         id = cur_group_id()) %>%
  mutate(.by = c(study, trial),
         trial = cur_group_id())

dat |>
  gtsummary::tbl_summary(statistic = list(gtsummary::all_continuous() ~ "mean = {mean}, SD = {sd}, [{min}, {max}]",
                                          gtsummary::all_categorical() ~ "n = {n} ({p}%)")) |>
  gtsummary::as_gt() |>
  gt::tab_style(style = gt::cell_text(align = "left"),
                locations = gt::cells_body())


#code to make a small version of the dataset
dat <- dat |>
  filter(study %in% sample(study, 20)) %>%
  filter(.by = study,
         id %in% sample(id, 10)) %>%
  mutate(.by = study,
         study = cur_group_id()) %>%
  mutate(.by = c(study, id),
         id = cur_group_id()) %>%
  mutate(.by = c(study, trial),
         trial = cur_group_id())
# 
saveRDS(dat, here::here("..", "Data Clean", "trihurdle-metaanalysis-more-predictors_dat-small_2026-03.15.rds"))
# dat <- readRDS(here::here("..", "Data Clean", "trihurdle-metaanalysis-more-predictors_dat-small.rds"))

dat %>% summary()


## Create trimmed version of DWOA (probably unnecessary)
dat$woa_winsor_trim <- dat$woa_winsor
dat$woa_winsor_trim[dat$woa_winsor == 1] <- .999
dat$woa_winsor_trim[dat$woa_winsor == 0] <- .001


## Subset predictors and create interaction terms
x <- dat %>%
  mutate(intercept = 1,
         # trial level
         distance = distance,
         confidence = zconfidence,
         distance_confidence = distance * confidence,
         # person level
         age = age,
         female = female,
         age_female = age * female,
         # study level
         student_judge = student_judge, 
         almanac = almanac,
         future = future, 
         expert_advisor = expert_advisor,
         incentive = incentive) %>%
  select(study, id, trial,
         intercept, 
         distance, confidence, distance_confidence,
         age, female, age_female, 
         student_judge, almanac, future, expert_advisor, incentive)


## Create test/toy data

testdat <- rbind(
  x |>
    select(trial, distance, confidence) |>
    pivot_longer(c(distance, confidence), names_to = "predictor", values_to = "value") |>
    summarize(.by = predictor,
              low = mean(value) - sd(value),
              med = mean(value),
              high = mean(value) + sd(value)),
  x |>
    select(id, age) |>
    unique() |>
    pivot_longer(c(age), names_to = "predictor", values_to = "value") |>
    # note that age low currently goes below observed in sample
    summarize(.by = predictor,
              low = mean(value) - sd(value),
              med = mean(value),
              high = mean(value) + sd(value))) |>
  pivot_longer(c(low, med, high), names_to = "which", values_to = "val") |>
  pivot_wider(names_from = predictor, values_from = val) |>
  select(-which) |>
  expand(distance, confidence,
         age, female = 0:1, 
         student_judge = 0:1,
         almanac = 0:1, future = 0:1, incentive = 0:1, expert_advisor = 0:1) |>
  mutate(distance_by_confidence = distance * confidence,
         age_by_female = age * female) |>
  select(distance, confidence, distance_by_confidence,
         age, female, age_by_female, 
         student_judge, almanac, future, expert_advisor, incentive)

3*3*3*2*2*2*2*2*2

conditions <- testdat |>
  select(!matches("_by_")) |>
  mutate(across(everything(), ~ ifelse(.x == min(.x), paste0(cur_column(), ".l"),
                                       ifelse(.x == max(.x), paste0(cur_column(), ".h"),
                                              paste0(cur_column(), ".m"))))) |>
  unique() |>
  mutate(condition_id = row_number()) |>
  nest(.by = condition_id, .key = "condition_levels") |>
  mutate(condition_name = map_vec(condition_levels, ~ .x |> paste(collapse = "_")))

condition_names <- conditions |>
  pluck("condition_name")


x <- x |> select(-c(study, id, trial))

## Scale priors

pr_v <- rep(NA, ncol(x) - 1) #no prior for the intercept?

for (i in 1:length(pr_v)){
  sdi <- sd(x[, i + 1], na.rm = T)
  if(length(levels(factor(x[, i]))) == 2) { # if binary?
    pr_v[i] <- 3
  }
  else {
    pr_v[i] <- 3 / sdi #why
  }
}


## Create DAC Variable

dat$DAC <- ifelse(dat$woa_winsor == 0, 2, 
                  ifelse(dat$woa_winsor == 1, 3, 
                         ifelse(dat$woa_winsor == 0.5, 4, 1)))



## Run model

helmer_stan_1 <- stan(here::here("Models", "trihurdle-metaanalysis_model-code.stan"),
                      data = list(N = nrow(dat), # number observations
                                  ncat = 4, #number of DAC categories
                                  Y1 = dat$DAC, # DAC variable
                                  K = ncol(x), #number of predictors + 1 for intercept
                                  X = x, #predictor matrix
                                  N_1 = length(unique(dat$id)), # number of unique participants
                                  M_1 = 1, # number of random effects for person, just intercepts here
                                  J_1 = dat$id, # participant ids
                                  N_2 = length(unique(dat$trial)), # number of unique items
                                  M_2 = 1, # number of random effects for items
                                  J_2 = dat$trial, # item ids
                                  
                                  N_3 = length(unique(dat$study)), # number of unique studies
                                  M_3 = 1, # number of random effects for studies
                                  J_3 = dat$study, # study ids
                                  
                                  Y2 = dat$woa_winsor_trim, #continuous response variable for beta regression
                                  Y2_complete = dat$woa_winsor, #untrimmed version
                                  Z_1_1 = rep(1, nrow(dat)), # random effect for person (just 1s for intercept)
                                  Z_2_1 = rep(1, nrow(dat)),  # random effect for item (just 1s for intercept)
                                  
                                  Z_3_1 = rep(1, nrow(dat)),  # random effect for study (just 1s for intercept)
                                  
                                  b_prior = pr_v, # scaled priors for coefficients
                                  prior_only = 0, # draw from prior only and ignore likelihood?
                                  N_test = nrow(testdat), # number of observations in test data
                                  X_test = testdat), # test data
                      warmup = 1000, iter = 2500,
                      seed = 50401, init_r = .2)

saveRDS(helmer_stan_1, file = here::here("Models", "trihurdle-metaanalysis_more-predictors_model.rds"))
saveRDS(testdat, file = here::here("..", "Data Clean", "testdat-more-predictors.rds"))
saveRDS(conditions, file = here::here("..", "Data Clean", "conditions_more-predictors.rds"))
saveRDS(condition_names, file = here::here("..", "Data Clean", "condition-names_more-predictors.rds"))
