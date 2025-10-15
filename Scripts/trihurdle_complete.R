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

dat_full <- read.csv(here::here("woa_datasets.csv"))

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
  mutate(.by = id,
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


## Run model

helmer_stan <- stan("DAC Helmer Tri-Hurdle.stan",
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
                                prior_only = 0, # draw from prior only and ignore likelihood?
                                N_test = nrow(testdat), # number of observations in test data
                                X_test = testdat), # test data
                    warmup = 1000, iter = 2000,
                    seed = 50401, init_r = .2)

saveRDS(helmer_stan, file = here::here("helmer_stan_tri-hurdle.rds"))
helmer_stan <- readRDS(here::here("helmer_stan_tri-hurdle.rds"))

traceplot(helmer_stan)

list_of_draws <- rstan::extract(helmer_stan, pars = "y_rep")
ppcplt <- ppc_dens_overlay(y = dat$woa_winsor_trim, yrep = list_of_draws$y_rep[1:50, ]) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Roboto", size = 12),
        plot.margin = margin(2, 10, 2, 10),
        legend.margin = margin(-10, 2, 2, 2),
        axis.line.x = element_line())
ppcplt

dat %>%
  mutate(study = factor(study)) %>%
  left_join(studynames,
            by = "study") %>%
  mutate(fif = ifelse(woa_winsor_trim == .5, 1, 0)) %>%
  mutate(.by = studyname,
         fifprop = sum(fif) / n()) %>%
  ggplot(aes(x = woa_winsor_trim)) +
  geom_density(fill = "#bc5090", color = NA) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 9, alpha = .3,
                 color = "#6a5188", fill = "white",
                 linewidth = 1) +
  facet_wrap(~ fct_reorder(studyname, -fifprop)) +
  scale_x_continuous("WOA", breaks = c(0, .5, 1)) +
  cowplot::theme_cowplot()



inits_dat <- data.frame(par = 1:nrow(data.frame(summary(helmer_stan, pars = "r_1_eta1_1")$summary)),
                        eta1 = data.frame(summary(helmer_stan, pars = "r_1_eta1_1")$summary)$mean +
                          data.frame(summary(helmer_stan, pars = "Intercept_eta1")$summary)$mean,
                        eta2 = data.frame(summary(helmer_stan, pars = "r_3_eta2_1")$summary)$mean +
                          data.frame(summary(helmer_stan, pars = "Intercept_eta2")$summary)$mean,
                        eta3 = data.frame(summary(helmer_stan, pars = "r_5_eta3_1")$summary)$mean +
                          data.frame(summary(helmer_stan, pars = "Intercept_eta3")$summary)$mean) %>% 
  mutate(Decline = exp(eta1) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Adopt = exp(eta2) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Compromise = 1 / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Midpoint = exp(eta3) / (1 + exp(eta1) + exp(eta2) + exp(eta3))) %>% 
  select(par, Decline, Adopt, Compromise, Midpoint) %>%
  pivot_longer(c("Decline", "Adopt", "Compromise", "Midpoint"),
               names_to = "Parameter", values_to = "P") %>%
  mutate(Parameter = factor(Parameter, levels = c("Decline", "Adopt", "Compromise", "Midpoint")))

ggplot(inits_dat, aes(x = Parameter, y = round(P, 2), fill = Parameter, color = Parameter)) + 
  geom_violin(adjust = 1, color = NA, alpha = .8) + 
  geom_jitter() +
  theme_minimal() +
  xlab("Intercept") +
  ylab("Probability") +
  guides(fill = "none") +
  scale_fill_manual(values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  scale_color_manual(values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  coord_cartesian(ylim = c(0, 1), expand = 0) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")













