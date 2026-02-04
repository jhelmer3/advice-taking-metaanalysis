
## setup ----

here::here()

library(rstan)
library(bayesplot)
library(tidyverse)

options(scipen = 999)


## reading ----

dat <- readRDS(here::here("..", "Data Clean", "trihurdle-metaanalysis_dat-small.rds"))
studynames <- readRDS(here::here("..", "Data Clean", "studynames.rds"))
condition_names <- readRDS(here::here("..", "Data Clean", "conditionnames.rds"))
testdat <- readRDS(here::here("..", "Data Clean", "testdat.rds"))
helmer_stan <- readRDS(here::here("Models", "trihurdle-metaanalysis_model-small.rds"))


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

traceplot(helmer_stan, pars = c("Intercept_eta3", "sd_6", "sd_8", "sd_9", "r_7_eta3_1", "r_8_eta3_1", "r_9_eta3_1"))


list_of_draws <- rstan::extract(helmer_stan, pars = "y_rep")
ppcplt <- ppc_dens_overlay(y = dat$woa_winsor, yrep = list_of_draws$y_rep[1:50, ]) +
  #coord_cartesian(ylim = c(0, 1)) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(2, 10, 2, 10),
        legend.margin = margin(-10, 2, 2, 2),
        axis.line.x = element_line())
ppcplt
saveRDS(ppcplt, here::here("Figures", "Tri-Hurdle Metaanalysis", "ppc_plt.rds"))
ggsave(ppcplt, filename = here::here("Figures", "Tri-Hurdle Metaanalysis", "ppc_plt.png"),
       width = 4, height = 2.5, dpi = 600)


ppcplt_g <- ppc_dens_overlay_grouped(y = dat$woa_winsor, yrep = list_of_draws$y_rep[1:50, ], 
                                     group = dat |>
                                       select(study) |> 
                                       left_join(studynames |> 
                                                   mutate(study = as.integer(study)), 
                                                 by = "study") |>
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
saveRDS(ppcplt_g, here::here("Figures", "Tri-Hurdle Metaanalysis", "ppc_plt_g.rds"))

ppcplt_g_pers <- ppc_dens_overlay_grouped(y = dat$woa_winsor, yrep = list_of_draws$y_rep[1:50, ], 
                                     group = dat |>
                                       pluck("id") |>
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




rstan::extract(helmer_stan, c("Intercept_eta1", "r_1_eta1_1")) |>
  as.data.frame() |> 
  pivot_longer(starts_with("r_1_eta1_1"), names_to = "person",
                           values_to = "est") |>
  mutate(person_effect_eta1 = Intercept_eta1 + est,
         person = str_split_i(person, "\\.", 2),
         
         .keep = "unused") |>
  View()

rstan::extract(helmer_stan, ) |> as.data.frame() |> names()

studylvl_dat |>
  summarize(.by = Parameter,
            mean = mean(P),
            sd = sd(P),
            range = list(range(P))) |>
  unnest(cols = range)


extract(helmer_stan, "r_9_eta3_1")


data.frame(par = 1:nrow(data.frame(summary(helmer_stan, pars = "r_5_eta1_1")$summary)),
           eta1 = data.frame(summary(helmer_stan, pars = "r_5_eta1_1")$summary)$mean +
             data.frame(summary(helmer_stan, pars = "Intercept_eta1")$summary)$mean,
           eta2 = data.frame(summary(helmer_stan, pars = "r_6_eta2_1")$summary)$mean +
             data.frame(summary(helmer_stan, pars = "Intercept_eta2")$summary)$mean,
           eta3 = data.frame(summary(helmer_stan, pars = "r_9_eta3_1")$summary)$mean +
             data.frame(summary(helmer_stan, pars = "Intercept_eta3")$summary)$mean)


playdat <- cbind(extr("A_t"),
                 select(extr("B_t"), b),
                 select(extr("pc1_t"), pc1),
                 select(extr("pc2_t"), pc2),
                 select(extr("pc3_t"), pc3)) %>%
  mutate(distance = case_when(substr(condition, 2, 2) == "L" ~ testdat[1, "distance"],
                              substr(condition, 2, 2) == "M" ~ testdat[2, "distance"],
                              substr(condition, 2, 2) == "H" ~ testdat[3, "distance"]),
         confidence = case_when(substr(condition, 4, 4) == "L" ~ testdat[1, "confidence"],
                                substr(condition, 4, 4) == "M" ~ testdat[4, "confidence"],
                                substr(condition, 4, 4) == "H" ~ testdat[7, "confidence"]))


playdat %>%
  pivot_longer(c("pc1", "pc2", "pc3"),
               names_to = "choice", values_to = "prob") %>%
  mutate(choice = factor(choice,
                         levels = c("pc2", "pc3", "pc1")),
         distance = factor(round(distance, 2), ordered = T),
         confidence = factor(round(confidence, 2), ordered = T)) %>% 
  filter(choice == "pc1") %>%
  ggplot(aes(y = prob, x = confidence, fill = distance)) +
  geom_violin(color = NA, width = .8, alpha = .8) + 
  stat_summary(aes(x = confidence, group = distance),
               fun = "mean",
               geom = "point",
               color = "white",
               position = position_dodge(.8)) +
  scale_x_discrete("confidence") +
  scale_y_continuous("P(Compromise)",
                     limits = c(0, 1)) +
  scale_fill_manual(values = c("#ffd6ed", "#df94be", "#bc5090")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")


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
