
## setup ----

here::here()

library(cmdstanr)
library(tidyverse)

options(scipen = 999)
options(mc.cores = parallel::detectCores())

set.seed(123)


## reading ----

dat <- readRDS(here::here("..", "Data Clean", "tri-m_more-pred_dat_2026-04-15.rds"))

## subset predictors and create interaction terms
x <- dat |>
  mutate(across(c(female, student_judge, almanac, future,
                expert_advisor, incentive),
                \(var) var - mean(var))) |>
  mutate(intercept = 1,
         # trial level
         distance = distance,
         # person level
         age = age,
         female = female,
         age_female = age * female,
         # study level
         studentjudge = student_judge, 
         almanac = almanac,
         future = future, 
         expertadvisor = expert_advisor,
         incentive = incentive) |>
  select(#study, id, trial,
         intercept, 
         distance,
         age, female, age_female, 
         studentjudge, almanac, future, expertadvisor, incentive)


## Create test/toy data

# testdat <- rbind(
#   x |>
#     select(trial, distance) |>
#     pivot_longer(c(distance), names_to = "predictor", values_to = "value") |>
#     summarize(.by = predictor,
#               low = mean(value) - sd(value),
#               med = mean(value),
#               high = mean(value) + sd(value)),
#   x |>
#     select(id, age) |>
#     unique() |>
#     pivot_longer(c(age), names_to = "predictor", values_to = "value") |>
#     summarize(.by = predictor,
#               low = mean(value) - sd(value),
#               med = mean(value),
#               high = mean(value) + sd(value))) |>
#   pivot_longer(c(low, med, high), names_to = "which", values_to = "val") |>
#   pivot_wider(names_from = predictor, values_from = val) |>
#   select(-which) |>
#   expand(distance,
#          age, female = 0:1,
#          studentjudge = 0:1,
#          almanac = 0:1, future = 0:1, incentive = 0:1, expertadvisor = 0:1) |>
#   mutate(age_by_female = age * female) |>
#   select(distance,
#          age, female, age_by_female,
#          studentjudge, almanac, future, expertadvisor, incentive)

testdat <- x |>
  select(-c(intercept, matches("_"))) |>
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
            select(-c(intercept, matches("_")), -any_of(var_name)) |> 
            summarize(across(everything(), 
                             \(var) if (var |> unique() |> length() > 3) mean(var) 
                             else 0)))) |>
  list_rbind() |> 
  select(-level) |>
  # specify interactions
  mutate(age_by_female = age * female,
         .after = female)

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

#x <- x |> select(-c(study, id, trial))

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

mod <- here::here("Models", "trihurdle-metaanalysis_model-code.stan") |>
  cmdstan_model()

fit <- mod$sample(data = list(N = nrow(dat), # number observations
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
                              X_test = testdat),
                  seed = 123,
                  chains = 4,
                  parallel_chains = 4,
                  iter_warmup = 500,
                  iter_sampling = 100,
                  refresh = 50)

# Load CmdStan output files into the fitted model object.
fit$draws() # Load posterior draws into the object.
try(fit$sampler_diagnostics(), silent = TRUE) # Load sampler diagnostics.

qs2::qs_save(fit, file = here::here("Models", "tri-m_more-pred_model_2.1.qs"))
saveRDS(testdat, file = here::here("..", "Data Clean", "tri-m_more-pred_testdat.rds"))
saveRDS(conditions, file = here::here("..", "Data Clean", "more-pred_conditions.rds"))
saveRDS(condition_names, file = here::here("..", "Data Clean", "more-pred_condition-names.rds"))
