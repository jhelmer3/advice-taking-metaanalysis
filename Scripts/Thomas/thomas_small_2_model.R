
## setup ----

here::here()

library(rstan)
library(tidyverse)

options(scipen = 999)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)

set.seed(123)


## reading ----

dat_small <- readRDS(here::here("..", "Data Clean", "Thomas", "thomas_small_dat_2026-04-20.rds"))

## subset predictors and create interaction terms
x <- dat_small |>
  mutate(intercept = 1) |>
  select(intercept, distance,
         age, female,
         big5_extraversion, big5_agreeableness, big5_conscientiousness,
         big5_neuroticism, big5_openness, npi_narcissism, anxiety) |>
  mutate(female = recode_values(female,
                                1 ~ 1,
                                0 ~ 0,
                                2 ~ 0),
         female = female - mean(female))


## Create test/toy data

testdat <- x |>
  select(-intercept) |>
  imap(\(var, var_name) 
       {
         if (var |> unique() |> length() > 2) {
           tibble(low = mean(var) - sd(var),
                  med = mean(var),
                  high = mean(var) + sd(var)) }
         else { tibble(low = min(var),
                       med = 0,
                       high = max(var))}
  } |>
    pivot_longer(everything(), names_to = "level", values_to = var_name) |>
    cbind(x |> 
            select(-intercept, -any_of(var_name)) |> 
            summarize(across(everything(), 
                             \(var) if (var |> unique() |> length() > 3) mean(var) 
                             else 0)))) |>
  list_rbind() |> 
  select(-level)

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

## Run model

helmer_stan_1 <- stan(here::here("Models", "trihurdle_model-code.stan"),
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
                                  
                                  Y2 = dat_small$woa_winsor_trim, #continuous response variable for beta regression
                                  Y2_complete = dat_small$woa_winsor, #untrimmed version
                                  Z_1_1 = rep(1, nrow(dat_small)), # random effect for person (just 1s for intercept)
                                  Z_2_1 = rep(1, nrow(dat_small)),  # random effect for item (just 1s for intercept)
                                  
                                  b_prior = pr_v, # scaled priors for coefficients
                                  prior_only = 0, # draw from prior only and ignore likelihood?
                                  N_test = nrow(testdat), # number of observations in test data
                                  X_test = testdat), # test data
                      warmup = 1000, iter = 2000,
                      seed = 50401, init_r = .2)
# 2026-04-22 started at 11:23am

saveRDS(helmer_stan_1, file = here::here("Models", "Thomas", "thomas_small_model.rds"))
saveRDS(testdat, file = here::here("..", "Data Clean", "Thomas", "thomas_small_testdat.rds"))
saveRDS(conditions, file = here::here("..", "Data Clean", "Thomas", "thomas_small_conditions.rds"))
saveRDS(condition_names, file = here::here("..", "Data Clean", "Thomas", "thomas_small_condition-names.rds"))

