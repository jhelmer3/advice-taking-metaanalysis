
## setup ----

here::here()

library(rstan)
library(cmdstanr)
library(tidyverse)

options(scipen = 999)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)

set.seed(123)


## reading ----

dat_small <- readRDS(here::here("..", "Data Clean", "tri-m_small_more-pred_dat_2026-04-15.rds"))

## subset predictors and create interaction terms
x <- dat_small |>
  mutate(intercept = 1,
         # trial level
         distance = distance,
         # person level
         age = age,
         female = female,
         age_female = age * female,
         # study level
         student_judge = student_judge, 
         almanac = almanac,
         future = future, 
         expert_advisor = expert_advisor,
         incentive = incentive) |>
  select(study, id, trial,
         intercept, 
         distance,
         age, female, age_female, 
         student_judge, almanac, future, expert_advisor, incentive)


## Create test/toy data

testdat <- rbind(
  x |>
    select(trial, distance) |>
    pivot_longer(c(distance), names_to = "predictor", values_to = "value") |>
    summarize(.by = predictor,
              low = mean(value) - sd(value),
              med = mean(value),
              high = mean(value) + sd(value)),
  x |>
    select(id, age) |>
    unique() |>
    pivot_longer(c(age), names_to = "predictor", values_to = "value") |>
    summarize(.by = predictor,
              low = mean(value) - sd(value),
              med = mean(value),
              high = mean(value) + sd(value))) |>
  pivot_longer(c(low, med, high), names_to = "which", values_to = "val") |>
  pivot_wider(names_from = predictor, values_from = val) |>
  select(-which) |>
  expand(distance,
         age, female = 0:1, 
         student_judge = 0:1,
         almanac = 0:1, future = 0:1, incentive = 0:1, expert_advisor = 0:1) |>
  mutate(age_by_female = age * female) |>
  select(distance,
         age, female, age_by_female, 
         student_judge, almanac, future, expert_advisor, incentive)

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


t1 <- Sys.time()
cmdstan_mod <- (here::here("Models", "trihurdle-metaanalysis_model-code.stan") |>
                  cmdstan_model())$sample(data = list(N = nrow(dat_small), # number observations
                                                      ncat = 4, #number of DAC categories
                                                      Y1 = dat_small$DAC, # DAC variable
                                                      K = ncol(x), #number of predictors + 1 for intercept
                                                      X = x, #predictor matrix
                                                      N_1 = length(unique(dat_small$id)), # number of unique participants
                                                      M_1 = 1, # number of random effects for person, just intercepts here
                                                      J_1 = dat_small$id, # participant ids
                                                      N_2 = length(unique(dat_small$trial)), # number of unique items
                                                      M_2 = 1, # number of random effects for items
                                                      J_2 = dat_small$trial, # item ids
                                                      
                                                      N_3 = length(unique(dat_small$study)), # number of unique studies
                                                      M_3 = 1, # number of random effects for studies
                                                      J_3 = dat_small$study, # study ids
                                                      
                                                      Y2 = dat_small$woa_winsor_trim, #continuous response variable for beta regression
                                                      Y2_complete = dat_small$woa_winsor, #untrimmed version
                                                      Z_1_1 = rep(1, nrow(dat_small)), # random effect for person (just 1s for intercept)
                                                      Z_2_1 = rep(1, nrow(dat_small)),  # random effect for item (just 1s for intercept)
                                                      
                                                      Z_3_1 = rep(1, nrow(dat_small)),  # random effect for study (just 1s for intercept)
                                                      
                                                      b_prior = pr_v, # scaled priors for coefficients
                                                      prior_only = 0, # draw from prior only and ignore likelihood?
                                                      N_test = nrow(testdat), # number of observations in test data
                                                      X_test = testdat),
                                          seed = 123,
                                          chains = 4,
                                          parallel_chains = 4,
                                          iter_warmup = 10,
                                          iter_sampling = 10,
                                          refresh = 2)
t2 <- Sys.time()

t3 <- Sys.time()
helmer_stan_1 <- stan(here::here("Models", "trihurdle-metaanalysis_model-code.stan"),
                      data = list(N = nrow(dat_small), # number observations
                                  ncat = 4, #number of DAC categories
                                  Y1 = dat_small$DAC, # DAC variable
                                  K = ncol(x), #number of predictors + 1 for intercept
                                  X = x, #predictor matrix
                                  N_1 = length(unique(dat_small$id)), # number of unique participants
                                  M_1 = 1, # number of random effects for person, just intercepts here
                                  J_1 = dat_small$id, # participant ids
                                  N_2 = length(unique(dat_small$trial)), # number of unique items
                                  M_2 = 1, # number of random effects for items
                                  J_2 = dat_small$trial, # item ids
                                  
                                  N_3 = length(unique(dat_small$study)), # number of unique studies
                                  M_3 = 1, # number of random effects for studies
                                  J_3 = dat_small$study, # study ids
                                  
                                  Y2 = dat_small$woa_winsor_trim, #continuous response variable for beta regression
                                  Y2_complete = dat_small$woa_winsor, #untrimmed version
                                  Z_1_1 = rep(1, nrow(dat_small)), # random effect for person (just 1s for intercept)
                                  Z_2_1 = rep(1, nrow(dat_small)),  # random effect for item (just 1s for intercept)
                                  
                                  Z_3_1 = rep(1, nrow(dat_small)),  # random effect for study (just 1s for intercept)
                                  
                                  b_prior = pr_v, # scaled priors for coefficients
                                  prior_only = 0, # draw from prior only and ignore likelihood?
                                  N_test = nrow(testdat), # number of observations in test data
                                  X_test = testdat), # test data
                      warmup = 10, iter = 20,
                      seed = 50401, init_r = .2)
t4 <- Sys.time()

paste("cmdstanr:", (t2 - t1))
paste("rstan:", (t4 - t3))
