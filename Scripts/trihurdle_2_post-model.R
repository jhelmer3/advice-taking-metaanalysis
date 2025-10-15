
## setup ----

here::here()

library(rstan)
library(bayesplot)
library(tidyverse)

options(scipen = 999)


## reading ----

dat <- readRDS(here::here("..", "Data Clean", "trihurdle_dat.rds"))
studynames <- readRDS(here::here("..", "Data Clean", "studynames.rds"))
condition_names <- readRDS(here::here("..", "Data Clean", "conditionnames.rds"))
testdat <- readRDS(here::here("..", "Data Clean", "testdat.rds"))
helmer_stan <- readRDS(here::here("Models", "trihurdle_model.rds"))


## checks ----

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
saveRDS(ppcplt, here::here("Figures", "Tri-Hurdle", "ppc_plt.rds"))

## results ----

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

personlvl_plt <- ggplot(inits_dat, aes(x = Parameter, y = round(P, 2), fill = Parameter, color = Parameter)) + 
  geom_violin(adjust = 1, color = NA, alpha = .8) + 
  geom_jitter() +
  theme_minimal() +
  xlab("Intercept") +
  ylab("Probability") +
  guides(fill = "none", color = "none") +
  scale_fill_manual(values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  scale_color_manual(values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  coord_cartesian(ylim = c(0, 1), expand = 0) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")
personlvl_plt
saveRDS(personlvl_plt, here::here("Figures", "Tri-Hurdle", "personlvl_plt.rds"))
