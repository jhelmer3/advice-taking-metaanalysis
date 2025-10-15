
## setup ----

here::here()

library(rstan)
library(bayesplot)
library(tidyverse)

options(scipen = 999)


## reading ----

dat <- readRDS(here::here("..", "Data Clean", "trihurdle-metaanalysis_dat.rds"))
studynames <- readRDS(here::here("..", "Data Clean", "studynames.rds"))
condition_names <- readRDS(here::here("..", "Data Clean", "conditionnames.rds"))
testdat <- readRDS(here::here("..", "Data Clean", "testdat.rds"))
helmer_stan <- readRDS(here::here("Models", "trihurdle-metaanalysis_model.rds"))


## checks ----

traceplot(helmer_stan)

list_of_draws <- rstan::extract(helmer_stan, pars = "y_rep")
ppcplt <- ppc_dens_overlay(y = dat$woa_winsor, yrep = list_of_draws$y_rep[1:50, ]) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(2, 10, 2, 10),
        legend.margin = margin(-10, 2, 2, 2),
        axis.line.x = element_line())
ppcplt
saveRDS(ppcplt, here::here("Figures", "Tri-Hurdle Metaanalysis", "ppc_plt.rds"))
ggsave(ppcplt, filename = here::here("Figures", "Tri-Hurdle Metaanalysis", "ppc_plt.png"),
       width = 4, height = 2.5, dpi = 600)

## results ----

p_DAC_forestplt <- helmer_stan %>%
  rstan::extract(pars = c("r_5_eta1_1", "r_6_eta2_1", "r_9_eta3_1", "Intercept_eta1", "Intercept_eta2", "Intercept_eta3")) %>%
  as.data.frame() %>% 
  pivot_longer(-c(Intercept_eta1, Intercept_eta2, Intercept_eta3),
               names_to = "study", values_to = "est") %>% 
  separate_wider_delim(study, ".", names = c("par", "study")) %>%
  mutate(.by = c(study, par),
         rep = row_number()) %>%
  pivot_wider(id_cols = c("study", "rep", "Intercept_eta1", "Intercept_eta2", "Intercept_eta3"),
              names_from = par, values_from = est) %>%
  mutate(eta1 = r_5_eta1_1 + Intercept_eta1,
         eta2 = r_6_eta2_1 + Intercept_eta2,
         eta3 = r_9_eta3_1 + Intercept_eta3,
         .keep = "unused") %>%
  left_join(studynames,
            by = "study") %>%
  mutate(Decline = exp(eta1) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Adopt = exp(eta2) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Compromise = 1 / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Midpoint = exp(eta3) /  (1 + exp(eta1) + exp(eta2) + exp(eta3))) %>% 
  pivot_longer(c(Decline, Adopt, Compromise, Midpoint),
               names_to = "par", values_to = "prob") %>%
  mutate(Parameter = factor(par, levels = c("Decline", "Adopt", "Compromise", "Midpoint"))) %>%
  {ggplot(., aes(x = prob, y = studyname, color = fct_inorder(par), fill = fct_inorder(par))) +
      geom_boxplot(width = .3, linewidth = 1.2, outliers = F, position = position_dodge(.5)) +
      stat_summary(geom = "point", fun = mean,
                   position = position_dodge(.5), size = 1.5, shape = 16) +
      scale_y_discrete(NULL) +
      scale_x_continuous(NULL, breaks = c(0, .25, .5, .75, 1),
                         labels = scales::percent_format()) +
      scale_color_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
      scale_fill_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
      coord_cartesian(xlim = c(0, 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            legend.position = "none")}
p_DAC_forestplt

p_DAC_forestplt_legend <- (data.frame(par = factor(c(1, 2, 3, 4), labels = c("Decline", "Adopt", "Compromise", "Midpoint")),
                                      y = c(1, 2, 3, 4)) %>%
                             ggplot(aes(x = par, y = y, color = par)) +
                             geom_point() +
                             scale_color_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
                             guides(color = guide_legend(override.aes = list(size = 4),
                                                         nrow = 2, byrow = T)) +
                             theme_minimal(base_size = 15) +
                             theme(legend.position = "top",
                                   legend.margin = margin(-10, 1, 1, 1, unit = "pt"))) %>%
  cowplot::get_plot_component('guide-box-top', return_all = TRUE)

saveRDS(p_DAC_forestplt, here::here("Figures", "Tri-Hurdle Metaanalysis", "p_DAC_forestplt.rds"))
saveRDS(p_DAC_forestplt_legend, here::here("Figures", "Tri-Hurdle Metaanalysis", "p_DAC_forestplt_legend.rds"))


# study level intercepts 
studylvl_dat <- data.frame(par = 1:nrow(data.frame(summary(helmer_stan, pars = "r_5_eta1_1")$summary)),
                          eta1 = data.frame(summary(helmer_stan, pars = "r_5_eta1_1")$summary)$mean +
                            data.frame(summary(helmer_stan, pars = "Intercept_eta1")$summary)$mean,
                          eta2 = data.frame(summary(helmer_stan, pars = "r_6_eta2_1")$summary)$mean +
                            data.frame(summary(helmer_stan, pars = "Intercept_eta2")$summary)$mean,
                          eta3 = data.frame(summary(helmer_stan, pars = "r_9_eta3_1")$summary)$mean +
                            data.frame(summary(helmer_stan, pars = "Intercept_eta3")$summary)$mean) %>% 
  mutate(Decline = exp(eta1) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Adopt = exp(eta2) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Compromise = 1 / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Midpoint = exp(eta3) /  (1 + exp(eta1) + exp(eta2) + exp(eta3))) %>% 
  select(par, Decline, Adopt, Compromise, Midpoint) %>%
  pivot_longer(c("Decline", "Adopt", "Compromise", "Midpoint"),
               names_to = "Parameter", values_to = "P") %>%
  mutate(Parameter = factor(Parameter, levels = c("Decline", "Adopt", "Compromise", "Midpoint")))
saveRDS(studylvl_dat, here::here("..", "Data Clean", "trihurdle-metaanalysis_stdylvl_dat.rds"))
studylvl_dat <- readRDS(here::here("..", "Data Clean", "trihurdle-metaanalysis_stdylvl_dat.rds"))

studylvl_plt <- ggplot(studylvl_dat, aes(x = Parameter, y = round(P, 2), fill = Parameter, color = Parameter)) + 
  geom_violin(color = NA, alpha = .6) + 
  geom_jitter(shape = 16, alpha = .8, width = .2) +
  theme_minimal() +
  xlab("Intercept") +
  ylab("Probability") +
  guides(fill = "none", color = "none") +
  scale_color_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  scale_fill_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  coord_cartesian(ylim = c(0, 1), expand = 0, clip = "off") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
studylvl_plt
saveRDS(studylvl_plt, here::here("Figures", "Tri-Hurdle Metaanalysis", "studylvl_plt.rds"))


extract(helmer_stan, "r_9_eta3_1")


data.frame(par = 1:nrow(data.frame(summary(helmer_stan, pars = "r_5_eta1_1")$summary)),
           eta1 = data.frame(summary(helmer_stan, pars = "r_5_eta1_1")$summary)$mean +
             data.frame(summary(helmer_stan, pars = "Intercept_eta1")$summary)$mean,
           eta2 = data.frame(summary(helmer_stan, pars = "r_6_eta2_1")$summary)$mean +
             data.frame(summary(helmer_stan, pars = "Intercept_eta2")$summary)$mean,
           eta3 = data.frame(summary(helmer_stan, pars = "r_9_eta3_1")$summary)$mean +
             data.frame(summary(helmer_stan, pars = "Intercept_eta3")$summary)$mean)








