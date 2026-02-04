
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

dat |> summarize(.by = study,
                 prop_m = mean(ifelse(woa_winsor == 0.5, 1, 0))) |>
  arrange(-prop_m) 

dat <- filter(dat, study == 22) %>%
  mutate(.by = id,
         id = cur_group_id()) %>%
  mutate(.by = c(study, trial),
         trial = cur_group_id())

dat %>% summary()

#saveRDS(dat, here::here("..", "Data Clean", "trihurdle_dat.rds"))
dat <- readRDS(here::here("..", "Data Clean", "trihurdle_dat.rds"))


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


condition_names <- expand.grid(distance = c("DL", "DM", "DH"),
                               confidence = c("CL", "CM", "CH")) %>%
  mutate(condition = paste0(distance, confidence)) %>%
  pull(condition)


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

k <- ncol(x)


trihurdle_stan <- "
data {
  
  int<lower = 1> N;  // number of observations
  int<lower = 2> ncat;  // number of DAC categories
  int Y1[N];  // categorical response variable for hurdle (D = 2, A = 3, or C = 1?) (midpoint 4)
  int<lower = 1> K;  // number of population-level effects post hurdle
  matrix[N, K] X;  // population-level design matrix post hurdle
  
  // data for group-level effects of ID 1 = person
  int<lower = 1> N_1;  // number of grouping levels
  int<lower = 1> M_1;  // number of coefficients per level (random effects)
  int<lower = 1> J_1[N];  // grouping indicator per observation
  vector[N] Z_1_1;  // group 1 (person)-level predictor values
  
  // data for group-level effects of ID 2
  int<lower = 1> N_2;  // number of grouping levels
  int<lower = 1> M_2;  // number of coefficients per level
  int<lower = 1> J_2[N];  // grouping indicator per observation
  vector[N] Z_2_1;  // group 2 (item)-level predictor values
  
  // data for outcomes
  vector[N] Y2;  // response variable
  vector[N] Y2_complete;  // response variable
  
  // priors
  vector[K - 1] b_prior;
  int prior_only;  // should the likelihood be ignored?
  
  // other stuff
  int<lower = 1> N_test;  // number of observations in test data
  matrix[N_test, K - 1] X_test;  // test data
  
}


transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}


parameters {
  
  vector[Kc] b_eta1;  // population-level effects
  real Intercept_eta1;  // temporary intercept for centered predictors
  vector[Kc] b_eta2;  // population-level effects
  real Intercept_eta2;  // temporary intercept for centered predictors
  vector[Kc] b_eta3;  // population-level effects for m hurdle
  real Intercept_eta3;  // temporary intercept for centered predictors for m hurdle
  
  vector<lower = 0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower = 0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
  vector<lower = 0>[M_1] sd_3;  // group-level standard deviations
  vector[N_1] z_3[M_1];  // standardized group-level effects
  vector<lower = 0>[M_2] sd_4;  // group-level standard deviations
  vector[N_2] z_4[M_2];  // standardized group-level effects
  vector<lower = 0>[M_1] sd_5;  // group-level standard deviations person-level for m hurdle
  vector[N_1] z_5[M_1];  // standardized group-level effects person-level for m hurdle
  vector<lower = 0>[M_2] sd_6;  // group-level standard deviations item-level for m hurdle
  vector[N_2] z_6[M_2];  // standardized group-level effects item-level for m hurdle
  
}

transformed parameters {
  vector[N_1] r_1_eta1_1;  // actual group-level effects
  vector[N_2] r_2_eta1_1;  // actual group-level effects
  vector[N_1] r_3_eta2_1;  // actual group-level effects
  vector[N_2] r_4_eta2_1;  // actual group-level effects
  
  vector[N_1] r_5_eta3_1;  // actual group-level effects for m hurdle
  vector[N_2] r_6_eta3_1;  // actual group-level effects for m hurdle
  
  r_1_eta1_1 = (sd_1[1] * (z_1[1]));
  r_2_eta1_1 = (sd_2[1] * (z_2[1]));
  r_3_eta2_1 = (sd_3[1] * (z_3[1]));
  r_4_eta2_1 = (sd_4[1] * (z_4[1]));
  
  r_5_eta3_1 = (sd_5[1] * (z_5[1]));
  r_6_eta3_1 = (sd_6[1] * (z_6[1]));

}


model {
  
  // initialize linear predictor term
  vector[N] eta1 = Intercept_eta1 + Xc * b_eta1;
  // initialize linear predictor term
  vector[N] eta2 = Intercept_eta2 + Xc * b_eta2;
  // initialize linear predictor term for m hurdle
  vector[N] eta3 = Intercept_eta3 + Xc * b_eta3;
  
  // linear predictor matrix
  vector[ncat] eta[N];
  
  for (n in 1:N) {
    // add more terms to the linear predictor
    eta1[n] += r_1_eta1_1[J_1[n]] * Z_1_1[n] + r_2_eta1_1[J_2[n]] * Z_2_1[n];
    // add more terms to the linear predictor
    eta2[n] += r_3_eta2_1[J_1[n]] * Z_1_1[n] + r_4_eta2_1[J_2[n]] * Z_2_1[n];
    // add more terms to the linear predictor
    eta3[n] += r_5_eta3_1[J_1[n]] * Z_1_1[n] + r_6_eta3_1[J_2[n]] * Z_2_1[n];
    
    eta[n] = [0, eta1[n], eta2[n], eta3[n]]';
  }
  
  // priors including all constants
  target += normal_lpdf(b_eta1 | 0, b_prior);
  target += normal_lpdf(b_eta2 | 0, b_prior);
  target += normal_lpdf(b_eta3 | 0, b_prior);
  
  target += normal_lpdf(Intercept_eta1 | 0, 5);
  target += normal_lpdf(Intercept_eta2 | 0, 5);
  target += normal_lpdf(Intercept_eta3 | 0, 5);
  
  target += student_t_lpdf(sd_1 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_1[1]);
  target += student_t_lpdf(sd_2 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_2[1]);
  target += student_t_lpdf(sd_3 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_3[1]);
  target += student_t_lpdf(sd_4 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_4[1]);
  
  target += gamma_lpdf(sd_5 | 1, 0.05);
  target += std_normal_lpdf(z_5[1]);
  target += gamma_lpdf(sd_6 | 1, 0.05);
  target += std_normal_lpdf(z_6[1]);
  
  
  // likelihood including all constants
  if (!prior_only) {  
    for (n in 1:N) {
      if (Y1[n] == 2){
        2 ~ categorical_logit(eta[n]);
      }
      else if (Y1[n] == 3){
        3 ~ categorical_logit(eta[n]);
      }
      else if (Y1[n] == 4){
        4 ~ categorical_logit(eta[n]);
      } 
    }
  }
}


generated quantities {
  // actual population-level intercept
  real b_eta1_Intercept = Intercept_eta1 - dot_product(means_X, b_eta1);
  // actual population-level intercept
  real b_eta2_Intercept = Intercept_eta2 - dot_product(means_X, b_eta2);
  // actual population-level intercept for m hurdle
  real b_eta3_Intercept = Intercept_eta3 - dot_product(means_X, b_eta3);
  
  
  // initialize linear predictor term
  real eta1;
  // initialize linear predictor term
  real eta2;
  // initialize linear predictor term for m hurdle
  real eta3;
  
  vector[N_test] eta1_t = b_eta1_Intercept + X_test * b_eta1;
  vector[N_test] eta2_t = b_eta2_Intercept + X_test * b_eta2;
  vector[N_test] eta3_t = b_eta3_Intercept + X_test * b_eta3;
  
  //testing results 
  real pc1_t[N_test];
  real pc2_t[N_test];
  real pc3_t[N_test];
  real pc4_t[N_test];
  
  vector[ncat] eta;
  vector[N] log_lik;
  real y_rep[N];
  real y1_rep;
  
  for (n in 1:N) {
    // add more terms to the linear predictor
    eta1 = Intercept_eta1 + Xc[n] * b_eta1 + r_1_eta1_1[J_1[n]] * Z_1_1[n] + r_2_eta1_1[J_2[n]] * Z_2_1[n];
    // add more terms to the linear predictor
    eta2 = Intercept_eta2 + Xc[n] * b_eta2 + r_3_eta2_1[J_1[n]] * Z_1_1[n] + r_4_eta2_1[J_2[n]] * Z_2_1[n];
    // add more terms to the linear predictor
    eta3 = Intercept_eta3 + Xc[n] * b_eta3 + r_5_eta3_1[J_1[n]] * Z_1_1[n] + r_6_eta3_1[J_2[n]] * Z_2_1[n];
    
    eta = [0, eta1, eta2, eta3]';  
    
    // add more terms to the linear predictor
    y1_rep = categorical_logit_rng(eta);
    if (y1_rep == 2){
      y_rep[n] = 0;
    } 
    else if (y1_rep == 3){
      y_rep[n] = 1;
    }
    else if (y1_rep == 4){
      y_rep[n] = 0.5;
    } else {
      y_rep[n] = 3;
    }
    
    if (Y1[n] == 2){
      log_lik[n] = categorical_logit_lpmf(2 | eta);
    }
    else if (Y1[n] == 3){
      log_lik[n] = categorical_logit_lpmf(3 | eta);
    }
    else if (Y1[n] == 4){
      log_lik[n] = categorical_logit_lpmf(4 | eta);
    }
    else {
      log_lik[n] = categorical_logit_lpmf(1 | eta);
    }
    
  }
  for (n2 in 1:N_test){
    pc2_t[n2] = exp(eta1_t[n2]) / (1 + exp(eta1_t[n2]) + exp(eta2_t[n2]) + exp(eta3_t[n2]));
    pc3_t[n2] = exp(eta2_t[n2]) / (1 + exp(eta1_t[n2]) + exp(eta2_t[n2]) + exp(eta3_t[n2]));
    pc4_t[n2] = exp(eta3_t[n2]) / (1 + exp(eta1_t[n2]) + exp(eta2_t[n2]) + exp(eta3_t[n2]));
    pc1_t[n2] = 1 - pc2_t[n2] - pc3_t[n2] - pc4_t[n2];
  }
}

"

helmer_stan <- stan(model_code = trihurdle_stan,
                    data = list(N = nrow(dat), # number observations
                                ncat = 4, #number of DAC categories
                                Y1 = dat$DAC, # DAC variable
                                K = k, #number of predictors + 1 for intercept
                                X = x, #predictor matrix
                                N_1 = length(unique(dat$id)), # number of unique participants
                                M_1 = 1, # number of random effects for person, just intercepts here
                                J_1 = dat$id, # participant ids
                                N_2 = length(unique(dat$trial)), # number of unique items
                                M_2 = 1, # number of random effects for items
                                J_2 = dat$trial, # item ids
                                
                                Y2 = dat$woa_winsor_trim, #continuous response variable for beta regression
                                Y2_complete = dat$woa_winsor, #untrimmed version
                                Z_1_1 = rep(1, nrow(dat)), # random effect for person (just 1s for intercept)
                                Z_2_1 = rep(1, nrow(dat)),  # random effect for item (just 1s for intercept)
                                
                                b_prior = pr_v, # scaled priors for coefficients
                                prior_only = i, # draw from prior only and ignore likelihood?
                                N_test = nrow(testdat), # number of observations in test data
                                X_test = testdat), # test data
                    warmup = 1000, iter = 3000,
                    seed = 50401, init_r = .2)

list_of_draws <- rstan::extract(helmer_stan, pars = "y_rep")
ppcplt <- ppc_dens_overlay(y = dat$woa_winsor_trim, yrep = list_of_draws$y_rep[1:100, ]) +
  xlim(0, 1) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Roboto", size = 12),
        plot.margin = margin(2, 10, 2, 10),
        legend.margin = margin(-10, 2, 2, 2),
        axis.line.x = element_line())
ppcplt

rstan::extract(helmer_stan, pars = c("pc1_t", "pc2_t", "pc3_t","pc4_t")) |>
  as.data.frame() |>
  pivot_longer(everything(),
               names_to = "par", values_to = "draw") |>
  separate_wider_delim(par, ".", names = c("par", "rep")) |>
  ggplot(aes(x = par, y = draw)) +
  geom_violin(scale = "width")

rstan::extract(helmer_stan, pars = c("eta1", "eta2", "eta3")) |>
  as.data.frame() |> 
  mutate(pc2 = exp(eta1) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         pc3 = exp(eta2) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         pc4 = exp(eta3) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         pc1 = 1 - pc2 - pc3 - pc4,
         .keep = "unused") |>
  pivot_longer(everything(),
               names_to = "par", values_to = "draw") |>
  ggplot(aes(x = par, y = draw)) +
  geom_violin(scale = "width")


rstan::extract(helmer_stan, pars = c("Intercept_eta1", "b_eta1")) |>
  as.data.frame() |>
  select(-c(b_eta1.2, b_eta1.3)) |>
  pivot_longer(everything(),
               names_to = "par", values_to = "draw") |>
  separate_wider_delim(par, ".", names = c("par", "rep"), too_few = "align_start") |>
  mutate(pc2 = exp(eta1) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         pc3 = exp(eta2) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         pc4 = exp(eta3) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         pc1 = 1 - pc2 - pc3 - pc4,
         .keep = "unused") |>
  ggplot(aes(x = par, y = draw)) +
  geom_violin(scale = "width")

rstan::extract(helmer_stan, pars = c("Intercept_eta1", "Intercept_eta2", "Intercept_eta3")) |>
  as.data.frame() |>
  summary()
rstan::extract(helmer_stan, pars = c("b_eta1", "b_eta1", "b_eta1")) |>
  as.data.frame() |>
  summary()
rstan::extract(helmer_stan, pars = c("r_1_eta1_1", "r_3_eta2_1", "r_5_eta3_1")) |>
  as.data.frame() |>
  mutate(pers_id = row_number()) |>
  pivot_longer(!pers_id,
               names_to = "par", values_to = "draw") |>
  separate_wider_delim(par, ".", names = c("par", "id")) |>
  pivot_wider(names_from = par, values_from = draw) |>
  select(-c(pers_id, id)) |>
  summary()
rstan::extract(helmer_stan, pars = c("r_2_eta1_1", "r_4_eta2_1", "r_6_eta3_1")) |>
  as.data.frame() |>
  mutate(pers_id = row_number()) |>
  pivot_longer(!pers_id,
               names_to = "par", values_to = "draw") |>
  separate_wider_delim(par, ".", names = c("par", "id")) |>
  pivot_wider(names_from = par, values_from = draw) |>
  select(-c(pers_id, id)) |>
  summary()



rstan::extract(helmer_stan, pars = c("r_1_eta1_1")) |>
  as.data.frame() |> 
  mutate(pers_id = row_number()) |>
  pivot_longer(!pers_id,
               names_to = "par", values_to = "draw") |>
  separate_wider_delim(par, ".", names = c("par", "id")) |>
  pivot_wider(names_from = par, values_from = draw) 


## barebones ----

trihurdle_barebones_stan <- "
data {
  
  int<lower = 1> N;  // number of observations
  int<lower = 2> ncat;  // number of DAC categories
  int Y1[N];  // categorical response variable for hurdle (D = 2, A = 3, or C = 1?) (midpoint 4)
  int<lower = 1> K;  // number of population-level effects post hurdle
  matrix[N, K] X;  // population-level design matrix post hurdle
  
  // data for outcomes
  vector[N] Y2;  // response variable
  vector[N] Y2_complete;  // response variable
  
  // priors
  vector[K - 1] b_prior;
  int prior_only;  // should the likelihood be ignored?
  
  // other stuff
  int<lower = 1> N_test;  // number of observations in test data
  matrix[N_test, K - 1] X_test;  // test data
  
}


transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}


parameters {
  
  vector[Kc] b_eta1;  // population-level effects
  real Intercept_eta1;  // temporary intercept for centered predictors
  vector[Kc] b_eta2;  // population-level effects
  real Intercept_eta2;  // temporary intercept for centered predictors
  vector[Kc] b_eta3;  // population-level effects for m hurdle
  real Intercept_eta3;  // temporary intercept for centered predictors for m hurdle
  
}

transformed parameters {

}


model {
  
  // initialize linear predictor term
  vector[N] eta1 = Intercept_eta1 + Xc * b_eta1;
  // initialize linear predictor term
  vector[N] eta2 = Intercept_eta2 + Xc * b_eta2;
  // initialize linear predictor term for m hurdle
  vector[N] eta3 = Intercept_eta3 + Xc * b_eta3;
  
  // linear predictor matrix
  vector[ncat] eta[N];
  
  // priors including all constants
  target += normal_lpdf(b_eta1 | 0, b_prior); 
  target += normal_lpdf(b_eta2 | 0, b_prior);
  target += normal_lpdf(b_eta3 | 0, b_prior);
  
  target += normal_lpdf(Intercept_eta1 | 0, 5);
  target += normal_lpdf(Intercept_eta2 | 0, 5);
  target += normal_lpdf(Intercept_eta3 | 0, 5);
  
  // likelihood including all constants
  if (!prior_only) {  
    for (n in 1:N) {
      if (Y1[n] == 2){
        2 ~ categorical_logit(eta[n]);
      }
      else if (Y1[n] == 3){
        3 ~ categorical_logit(eta[n]);
      }
      else if (Y1[n] == 4){
        4 ~ categorical_logit(eta[n]);
      } 
    }
  }
}


generated quantities {
  // actual population-level intercept
  real b_eta1_Intercept = Intercept_eta1 - dot_product(means_X, b_eta1);
  // actual population-level intercept
  real b_eta2_Intercept = Intercept_eta2 - dot_product(means_X, b_eta2);
  // actual population-level intercept for m hurdle
  real b_eta3_Intercept = Intercept_eta3 - dot_product(means_X, b_eta3);
  
  
  // initialize linear predictor term
  real eta1;
  // initialize linear predictor term
  real eta2;
  // initialize linear predictor term for m hurdle
  real eta3;
  
  vector[N_test] eta1_t = b_eta1_Intercept + X_test * b_eta1;
  vector[N_test] eta2_t = b_eta2_Intercept + X_test * b_eta2;
  vector[N_test] eta3_t = b_eta3_Intercept + X_test * b_eta3;
  
  //testing results 
  real pc1_t[N_test];
  real pc2_t[N_test];
  real pc3_t[N_test];
  real pc4_t[N_test];
  
  vector[ncat] eta;
  vector[N] log_lik;
  real y_rep[N];
  real y1_rep;
  
  for (n in 1:N) {
    // add more terms to the linear predictor
    eta1 = Intercept_eta1 + Xc[n] * b_eta1;
    // add more terms to the linear predictor
    eta2 = Intercept_eta2 + Xc[n] * b_eta2;
    // add more terms to the linear predictor
    eta3 = Intercept_eta3 + Xc[n] * b_eta3;
    
    eta = [0, eta1, eta2, eta3]';  
    
    // add more terms to the linear predictor
    y1_rep = categorical_logit_rng(eta);
    if (y1_rep == 2){
      y_rep[n] = 0;
    } 
    else if (y1_rep == 3){
      y_rep[n] = 1;
    }
    else if (y1_rep == 4){
      y_rep[n] = 0.5;
    } else {
      y_rep[n] = 3;
    }
    
    if (Y1[n] == 2){
      log_lik[n] = categorical_logit_lpmf(2 | eta);
    }
    else if (Y1[n] == 3){
      log_lik[n] = categorical_logit_lpmf(3 | eta);
    }
    else if (Y1[n] == 4){
      log_lik[n] = categorical_logit_lpmf(4 | eta);
    }
    else {
      log_lik[n] = categorical_logit_lpmf(1 | eta);
    }
    
  }
  for (n2 in 1:N_test){
    pc2_t[n2] = exp(eta1_t[n2]) / (1 + exp(eta1_t[n2]) + exp(eta2_t[n2]) + exp(eta3_t[n2]));
    pc3_t[n2] = exp(eta2_t[n2]) / (1 + exp(eta1_t[n2]) + exp(eta2_t[n2]) + exp(eta3_t[n2]));
    pc4_t[n2] = exp(eta3_t[n2]) / (1 + exp(eta1_t[n2]) + exp(eta2_t[n2]) + exp(eta3_t[n2]));
    pc1_t[n2] = 1 - pc2_t[n2] - pc3_t[n2] - pc4_t[n2];
  }
}

"

helmer_stan <- stan(model_code = trihurdle_barebones_stan,
                    data = list(N = nrow(dat), # number observations
                                ncat = 4, #number of DAC categories
                                Y1 = dat$DAC, # DAC variable
                                K = k, #number of predictors + 1 for intercept
                                X = x, #predictor matrix
                                
                                Y2 = dat$woa_winsor_trim, #continuous response variable for beta regression
                                Y2_complete = dat$woa_winsor, #untrimmed version
                                Z_1_1 = rep(1, nrow(dat)), # random effect for person (just 1s for intercept)
                                Z_2_1 = rep(1, nrow(dat)),  # random effect for item (just 1s for intercept)
                                
                                b_prior = pr_v, # scaled priors for coefficients
                                prior_only = 0, # draw from prior only and ignore likelihood?
                                N_test = nrow(testdat), # number of observations in test data
                                X_test = testdat), # test data
                    warmup = 1000, iter = 3000,
                    seed = 50401, init_r = .2)

list_of_draws <- rstan::extract(helmer_stan, pars = "y_rep")
ppcplt <- ppc_dens_overlay(y = dat$woa_winsor_trim, yrep = list_of_draws$y_rep[1:100, ]) +
  xlim(0, 1) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Roboto", size = 12),
        plot.margin = margin(2, 10, 2, 10),
        legend.margin = margin(-10, 2, 2, 2),
        axis.line.x = element_line())
ppcplt
