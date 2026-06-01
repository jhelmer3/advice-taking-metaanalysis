## setup ----

here::here()

library(rstan)
library(bayesplot)
library(tidyverse)
library(patchwork)

options(scipen = 999)


## reading ----

dat <- readRDS(here::here("..", "Data Clean", "tri-m_more-pred_dat_2026-04-15.rds"))
studynames <- readRDS(here::here("..", "Data Clean", "studynames.rds"))
conditions <- readRDS(here::here("..", "Data Clean", "more-pred_conditions.rds"))
condition_names <- readRDS(here::here("..", "Data Clean", "more-pred_condition-names.rds"))
helmer_stan <- readRDS(here::here("Models", "tri-m_more-pred_model.rds"))

predictors <- conditions |>
  select(-c(condition_id, condition_name)) |>
  head(1) |>
  unnest(condition_levels) |>
  names()

## functions ----

extr <- function(var) {
  helmer_stan %>% 
    rstan::extract(var) %>%
    data.frame() %>%
    select(1:length(condition_names)) %>%
    rename_all(~ condition_names) %>%
    pivot_longer(everything(),
                 names_to = "condition", values_to = substr(tolower(var), 1, nchar(var) - 2))
}


## checks ----

traceplot(helmer_stan, pars = c("Intercept_eta3"))


list_of_draws <- rstan::extract(helmer_stan, pars = "y_rep")
ppcplt <- ppc_dens_overlay(y = dat$woa_winsor, yrep = list_of_draws$y_rep[1:50, ]) +
  #coord_cartesian(ylim = c(0, 1)) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(2, 10, 2, 10),
        legend.margin = margin(-10, 2, 2, 2),
        axis.line.x = element_line())
ppcplt
saveRDS(ppcplt, here::here("Figures", "Tri-Hurdle Metaanalysis More Predictors", "ppc_plt.rds"))
ggsave(ppcplt, filename = here::here("Figures", "Tri-Hurdle Metaanalysis More Predictors", "ppc_plt.png"),
       width = 4, height = 2.5, dpi = 600)


ppcplt_g <- ppc_dens_overlay_grouped(y = dat$woa_winsor, yrep = list_of_draws$y_rep[1:50, ], 
                                     group = dat |>
                                       pluck("studyname") |>
                                       as.factor()) +
  scale_x_continuous(breaks = c(0, .5, 1),
                     labels = c("0", ".5", "1")) +
  guides(x = guide_axis(cap = "both")) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(2, 10, 2, 10),
        legend.margin = margin(-10, 2, 2, 2),
        axis.line.y = element_blank(),
        strip.background = element_blank())
ppcplt_g
saveRDS(ppcplt_g, here::here("Figures", "Tri-Hurdle Metaanalysis More Predictors", "ppc_plt_g.rds"))

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
  mutate(eta1 = Intercept_eta1 + r_5_eta1_1,
         eta2 = Intercept_eta2 + r_6_eta2_1,
         eta3 = Intercept_eta3 + r_9_eta3_1,
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


playdat_full <- cbind(extr("A_t"),
                      select(extr("B_t"), b),
                      select(extr("pc1_t"), pc1),
                      select(extr("pc2_t"), pc2),
                      select(extr("pc3_t"), pc3),
                      select(extr("pc4_t"), pc4))

saveRDS(playdat_full, here::here("..", "Data Clean", "tri-m_small_more-pred_playdat.rds"))

playdat <- playdat_full %>%
  mutate(.by = condition,
         rep = row_number()) |>
  # getting just confidence and distance to start
  filter(str_detect(condition,
                    paste0(predictors[3], ".m_") |>
                      paste0(... = _,  paste0(predictors[4:length(predictors)], collapse = ".l_")) |>
                      paste0(... = _, ".l"))) |>
  separate_wider_regex(cols = condition,
                       patterns = c("^distance\\.", distance = "[lmh]",
                                    "_confidence\\.", confidence = "[lmh]", ".*")) %>%
  mutate(distance = factor(distance, levels = c("l", "m", "h"),
                           labels = c("-1 SD", "Mean", "+1 SD"), ordered = T),
         confidence = factor(confidence, levels = c("l", "m", "h"),
                             labels = c("-1 SD", "Mean", "+1 SD"), ordered = T))

saveRDS(playdat, here::here("..", "Data Clean", "trihurdle-metaanalysis_more-predictors_playdat.rds"))


predplt_dat <- playdat %>%
  pivot_longer(c("pc1", "pc2", "pc3", "pc4"),
               names_to = "choice", values_to = "prob") %>%
  mutate(choice = factor(choice,
                         levels = c("pc2", "pc3", "pc4", "pc1"))) 


playdat |>
  mutate(beta = pmap_dbl(list(pc1, pc2, pc3, pc4, a, b), \(pc1, pc2, pc3, pc4, a, b) {
    data.frame(res = rmultinom(1, 1, c(pc1, pc2, pc3, pc4)),
               woa = c(rbeta(1, a, b), 0, 1, 0.5)) |>
      filter(res == 1) |>
      pull(woa)
  }))


data.frame(res = rmultinom(1, 1, c(0.0104, 0.156, 0.832, 0.00198)),
           woa = c(rbeta(1, 1.32, 1.54), 0, 1, 0.5)) |>
  filter(res == 1) |>
  pull(woa)


test_rbeta_gen <- matrix(NA, ncol = 1, nrow = nrow(playdat))

set.seed(203001)
for (i in 1:nrow(playdat)){
  for(j in 1:ncol(test_rbeta_gen)){
    p <- rmultinom(1, 1, c(playdat$pc1[i], playdat$pc2[i], playdat$pc3[i], playdat$pc4[i]))
    if (p[2] == 1){
      test_rbeta_gen[i, j] <- 0
    }
    else if (p[3] == 1){
      test_rbeta_gen[i, j] <- 1
    }
    else if (p[4] == 1){
      test_rbeta_gen[i, j] <- 0.5
    }
    else{
      test_rbeta_gen[i, j] <- rbeta(1, playdat$a[i], playdat$b[i])
    }
  }
}

test_rbeta <- data.frame(test_rbeta_gen) %>%
  cbind(playdat) %>%
  select(test_rbeta_gen, distance, confidence) %>%
  rename(dens = test_rbeta_gen)

test_rbeta <- playdat |>
  mutate(woa = pmap_dbl(list(pc1, pc2, pc3, pc4, a, b), \(pc1, pc2, pc3, pc4, a, b) {
    data.frame(res = rmultinom(1, 1, c(pc1, pc2, pc3, pc4)),
               woa = c(rbeta(1, a, b), 0, 1, 0.5)) |>
      filter(res == 1) |>
      pull(woa)
  }))

test_rbeta_mean <- test_rbeta %>%
  filter(woa > 0 & woa < 1) %>%
  summarise(.by = c(distance, confidence),
            woa = mean(woa))


predictor_plt <- (predplt_dat |> 
                    ggplot(aes(y = prob, x = confidence, fill = distance, color = distance)) +
                    geom_violin(alpha = .8) + 
                    stat_summary(aes(x = confidence, group = distance),
                                 fun = "mean",
                                 geom = "point",
                                 position = position_dodge(0.9)) +
                    scale_x_discrete(NULL, labels = c("-0.98" = "-1 SD", "0" = "Mean", "0.98" = "+1 SD")) +
                    scale_y_continuous("P",
                                       limits = c(0, 1)) +
                    scale_fill_manual(values = c("#ffd6ed", "#df94be", "#bc5090")) +
                    scale_color_manual(values = scales::col_darker(c("#ffd6ed", "#df94be", "#bc5090"))) +
                    facet_wrap(~ choice, labeller = as_labeller(
                      c(pc1 = "Compromise", pc2 = "Decline", pc3 = "Adopt", pc4 = "Midpoint")),
                      nrow = 1) +
                    theme_minimal(base_size = 12) +
                    theme(panel.grid.minor = element_blank(),
                          panel.grid.major.x = element_blank(),
                          legend.position = "none",
                          aspect.ratio = 1)) /
  (test_rbeta %>%
     ggplot(aes(x = confidence, y = woa, fill = distance, color = distance)) + 
     geom_violin(alpha = .8,
                 position = position_dodge(.9)) +  
     geom_point(data = test_rbeta_mean,
                aes(x = confidence, y = woa),
                position = position_dodge(.9),
                size = 2) +
     stat_summary(aes(x = confidence, group = distance),
                  fun = "mean",
                  geom = "point",
                  shape = 21,
                  fill = "white",
                  size = 2.5,
                  position = position_dodge(.9)) +
     labs(fill = "Distance", x = "Confidence", y = "WOA") +
     scale_x_discrete(labels = c("-0.98" = "-1 SD", "0" = "Mean", "0.98" = "+1 SD")) +
     scale_fill_manual(values = c("#ffd6ed", "#df94be", "#bc5090"),
                       labels = c("-0.02" = "-1 SD", "0.32" = "Mean", "0.66" = "+1 SD")) +
     scale_color_manual("Distance", 
                        values = scales::col_darker(c("#ffd6ed", "#df94be", "#bc5090")),
                        labels = c("-0.02" = "-1 SD", "0.32" = "Mean", "0.66" = "+1 SD")) +
     theme_minimal(base_size = 12) +
     theme(panel.grid.minor = element_blank(),
           panel.grid.major.x = element_blank(),
           legend.position = "bottom"))

saveRDS(predictor_plt, here::here("Figures", "Tri-Hurdle Metaanalysis More Predictors", "predictor_plt.rds"))




helmer_stan %>% 
  rstan::extract("pc1_t") %>%
  pluck(var) %>%
  data.frame()


test_rbeta_gen <- matrix(NA, ncol = 1, nrow = nrow(playdat))

set.seed(203001)
for (i in 1:nrow(playdat)){
  for(j in 1:ncol(test_rbeta_gen)){
    p <- rmultinom(1, 1, c(playdat$pc1[i], playdat$pc2[i], playdat$pc3[i]))
    if (p[2] == 1){
      test_rbeta_gen[i, j] <- 0
    }
    else if (p[3] == 1){
      test_rbeta_gen[i, j] <- 1
    }
    else{
      test_rbeta_gen[i, j] <- rbeta(1, playdat$a[i], playdat$b[i])
    }
  }
}

test_rbeta <- data.frame(test_rbeta_gen) %>%
  cbind(select(playdat, condition, distance, confidence)) %>%
  select(condition, test_rbeta_gen, distance, confidence) %>%
  rename(dens = test_rbeta_gen)

test_rbeta_mean <- test_rbeta %>%
  filter(dens > 0 & dens < 1) %>%
  summarise(.by = c(distance, confidence),
            dens = mean(dens)) %>%
  mutate(distance = factor(round(distance, 2), ordered = T),
         confidence = factor(round(confidence, 2), ordered = T))


test_rbeta %>%
  mutate(distance = factor(round(distance, 2), ordered = T),
         confidence = factor(round(confidence, 2), ordered = T)) %>% 
  ggplot(aes(x = confidence, y = dens, fill = distance)) + 
  geom_violin(width = .5, color = NA, alpha = .8,
              position = position_dodge(.6)) +  
  geom_point(data = test_rbeta_mean,
             aes(x = confidence, y = dens),
             position = position_dodge(.6),
             size = 2) +
  stat_summary(aes(x = confidence, group = distance),
               fun = "mean",
               geom = "point",
               color = "gray40",
               shape = 21,
               fill = "white",
               position = position_dodge(.6)) +
  labs(fill = "distance", x = "confidence", y = "WOA") +
  scale_fill_manual(values = c("#ffd6ed", "#df94be", "#bc5090")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")


helmer_stan %>%
  rstan::extract(pars = c("r_5_eta1_1", "r_6_eta2_1", "r_9_eta3_1", "Intercept_eta1", "Intercept_eta2", "Intercept_eta3")) %>%
  as.data.frame() %>% 
  pivot_longer(-c(Intercept_eta1, Intercept_eta2, Intercept_eta3),
               names_to = "study", values_to = "est") %>% 
  separate_wider_delim(study, ".", names = c("par", "study")) %>%
  mutate(.by = c(study, par),
         rep = row_number()) %>%
  pivot_wider(id_cols = c("study", "rep", "Intercept_eta1", "Intercept_eta2", "Intercept_eta3"),
              names_from = par, values_from = est) %>%
  mutate(eta1 = Intercept_eta1 + r_5_eta1_1,
         eta2 = Intercept_eta2 + r_6_eta2_1,
         eta3 = Intercept_eta3 + r_9_eta3_1,
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
  ggplot(aes(x = prob, y = studyname, color = fct_rev(par), fill = fct_rev(par))) +
  ggridges::geom_density_ridges() +
  scale_y_discrete(NULL) +
  scale_x_continuous(NULL, breaks = c(0, .25, .5, .75, 1),
                     labels = scales::percent_format()) +
  scale_color_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  scale_fill_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")
