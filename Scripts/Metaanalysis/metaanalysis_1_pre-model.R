
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

saveRDS(studynames, here::here("..", "Data Clean", "studynames.rds"))

dat <- dat_full %>%
  select(!c(gender, published, base, female_percentage,
            mean_age, student_judge, almanac, future,
            incentive, abs_value, winsor)) %>%
  mutate(distance = abs(firstestimate - advice) / firstestimate) %>%
  filter(!is.na(woa_raw) & !is.na(zconfidence) & distance < 1) %>%
  mutate(.by = study,
         study = cur_group_id()) %>%
  mutate(.by = c(study, id),
         id = cur_group_id()) %>%
  mutate(.by = c(study, trial),
         trial = cur_group_id())

dat %>% summary()


## Create trimmed version of DWOA (probably unnecessary)
dat$woa_winsor_trim <- dat$woa_winsor
dat$woa_winsor_trim[dat$woa_winsor == 1] <- .999
dat$woa_winsor_trim[dat$woa_winsor == 0] <- .001


## Subset predictors and create interaction terms
x <- dat %>%
  mutate(intercept = 1,
         distance = distance,
         confidence = zconfidence,
         distance_confidence = distance * confidence) %>%
  select(intercept, distance, confidence, distance_confidence)


## Create test/toy data

testdat <- expand.grid(distance = c(mean(x$distance) - sd(x$distance),
                                    mean(x$distance),
                                    mean(x$distance) + sd(x$distance)),
                       confidence = c(mean(x$confidence) - sd(x$confidence),
                                      mean(x$confidence),
                                      mean(x$confidence) + sd(x$confidence))) %>%
  mutate(distance_confidence = distance * confidence)
saveRDS(testdat, here::here("..", "Data Clean", "testdat.rds"))

condition_names <- expand.grid(distance = c("DL", "DM", "DH"),
                               confidence = c("CL", "CM", "CH")) %>%
  mutate(condition = paste0(distance, confidence)) %>%
  pull(condition)
saveRDS(condition_names, here::here("..", "Data Clean", "conditionnames.rds"))

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
                         1))

k <- ncol(x)


saveRDS(dat, here::here("..", "Data Clean", "metaanalysis_dat.rds"))

## Run model

helmer_stan <- stan(here::here("Models", "metaanalysis_model-code.stan"),
                    data = list(N = nrow(dat), # number observations
                                ncat = 3, #number of DAC categories
                                Y1 = dat$DAC, # DAC variable
                                K = k, #number of predictors + 1 for intercept
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

saveRDS(helmer_stan, file = here::here("helmer_stan_2_4.rds"))
