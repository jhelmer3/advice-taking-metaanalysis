
## setup ----

here::here()

library(rstan)
library(bayesplot)
library(tidyverse)

options(scipen = 999)


## reading ----

dat <- readRDS(here::here("..", "Data Clean", "metaanalysis_dat.rds"))
studynames <- readRDS(here::here("..", "Data Clean", "studynames.rds"))
condition_names <- readRDS(here::here("..", "Data Clean", "conditionnames.rds"))
testdat <- readRDS(here::here("..", "Data Clean", "testdat.rds"))
helmer_stan <- readRDS(here::here("Models", "metaanalysis_model.rds"))


## checks ----

traceplot(helmer_stan)

list_of_draws <- rstan::extract(helmer_stan, pars = "y_rep")
ppcplt <- ppc_dens_overlay(y = dat$woa_winsor_trim, yrep = list_of_draws$y_rep[1:50, ]) +
  theme(legend.position = "bottom",
        text = element_text(family = "Roboto"),
        plot.margin = margin(2, 10, 2, 10),
        legend.margin = margin(-10, 2, 2, 2))
ppcplt
saveRDS(ppcplt, here::here("Figures", "Metaanalysis", "ppc_plt.rds"))


## results ----

topfifs_plt <- dat %>%
  mutate(study = factor(study)) %>%
  left_join(studynames,
            by = "study") %>%
  mutate(fif = ifelse(woa_winsor_trim == .5, 1, 0)) %>%
  summarize(.by = studyname,
            fifprop = sum(fif) / n()) %>%
  filter(fifprop > .08) %>%
  arrange(desc(fifprop)) %>%
  ggplot(aes(y = fct_reorder(studyname, fifprop), x = fifprop)) +
  geom_col(fill = "#ff926d", alpha = .8) +
  scale_x_continuous("Proportion of .5s", labels = scales::percent_format()) +
  labs(y = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linewidth = 1, linetype = "dashed"),
        panel.grid.major.y = element_blank())
topfifs_plt

dat %>%
  mutate(study = factor(study)) %>%
  left_join(studynames,
            by = "study") %>%
  mutate(fif = ifelse(woa_winsor_trim == .5, 1, 0)) %>%
  summarize(.by = studyname,
            fifprop = sum(fif) / n()) %>%
  saveRDS(file = here::here("..", "Data Clean", "topfifs_dat.rds"))

saveRDS(topfifs_plt, here::here("Figures", "Metaanalysis", "topfifs_plt.rds"))

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


extr <- function(var) {
  helmer_stan %>% 
    rstan::extract(var) %>%
    pluck(var) %>%
    data.frame() %>%
    select(1:length(condition_names)) %>%
    rename_all(~ condition_names) %>%
    pivot_longer(everything(),
                 names_to = "condition", values_to = substr(tolower(var), 1, nchar(var) - 2))
}

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
  geom_hline(yintercept = .5, color = "gray60") + 
  geom_violin(width = .5, color = NA, alpha = .8,
              position = position_dodge(.9)) +  
  geom_point(data = test_rbeta_mean,
             aes(x = confidence, y = dens),
             position = position_dodge(.9),
             size = 2) +
  stat_summary(aes(x = confidence, group = distance),
               fun = "mean",
               geom = "point",
               color = "gray40",
               shape = 21,
               fill = "white",
               position = position_dodge(.9)) +
  labs(fill = "distance", x = "confidence", y = "WOA") +
  scale_fill_manual(values = c("#ffd6ed", "#df94be", "#bc5090")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")


inits_dat <- data.frame(par = 1:nrow(data.frame(summary(helmer_stan, pars = "r_1_eta1_1")$summary)),
                        eta1 = data.frame(summary(helmer_stan, pars = "r_1_eta1_1")$summary)$mean +
                          data.frame(summary(helmer_stan, pars = "Intercept_eta1")$summary)$mean,
                        eta2 = data.frame(summary(helmer_stan, pars = "r_3_eta2_1")$summary)$mean +
                          data.frame(summary(helmer_stan, pars = "Intercept_eta2")$summary)$mean) %>% 
  mutate(Decline = exp(eta1)/(1 + exp(eta1) + exp(eta2)),
         Adopt = exp(eta2)/(1 + exp(eta1) + exp(eta2)),
         Compromise = 1/(1 + exp(eta1) + exp(eta2))) %>% 
  select(par, Decline, Adopt, Compromise) %>%
  pivot_longer(c("Decline", "Adopt", "Compromise"),
               names_to = "Parameter", values_to = "P") %>%
  mutate(Parameter = factor(Parameter, levels = c("Decline", "Adopt", "Compromise")))


ggplot(inits_dat, aes(x = Parameter, y = round(P, 2), fill = Parameter)) + 
  geom_violin(adjust = 1, color = NA, alpha = .8) + 
  theme_minimal() +
  xlab("Intercept") +
  ylab("Probability") +
  guides(fill = "none") +
  scale_fill_manual(values = c("#003f5c", "#bc5090", "#ffa600")) +
  coord_cartesian(ylim = c(0, 1), expand = 0) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")

helmer_stan %>%
  rstan::extract(pars = c("r_5_eta1_1", "r_6_eta2_1")) %>%
  as.data.frame() %>% 
  pivot_longer(everything(),
               names_to = "study", values_to = "est") %>%
  separate_wider_delim(study, ".", names = c("par", "study")) %>%
  filter(study %in% 1:10) %>%
  left_join(studynames,
            by = "study") %>%
  mutate(odds = exp(est),
         prob = odds / (1 + odds)) %>%
  ggplot(aes(x = prob, y = studyname, color = par, fill = par)) +
  geom_boxplot(width = .25, linewidth = .8, outliers = F, position = position_dodge(.5)) +
  stat_summary(geom = "point", fun = mean, color = "white", position = position_dodge(.5), size = 1.5, shape = 16) +
  scale_y_discrete(NULL) +
  scale_x_continuous("estimate", breaks = c(0, .25, .5, .75, 1),
                     labels = scales::percent_format()) +
  scale_color_manual(NULL, values = c("#6a5188", "#c77998")) +
  scale_fill_manual(NULL, values = c("#6a5188", "#c77998")) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

p_DAC_forestplt <- helmer_stan %>%
  rstan::extract(pars = c("r_5_eta1_1", "r_6_eta2_1", "Intercept_eta1", "Intercept_eta2")) %>%
  as.data.frame() %>% 
  pivot_longer(-c(Intercept_eta1, Intercept_eta2),
               names_to = "study", values_to = "est") %>% 
  separate_wider_delim(study, ".", names = c("par", "study")) %>%
  mutate(.by = c(study, par),
         rep = row_number()) %>%
  pivot_wider(id_cols = c("study", "rep", "Intercept_eta1", "Intercept_eta2"),
              names_from = par, values_from = est) %>%
  mutate(eta1 = r_5_eta1_1 + Intercept_eta1,
         eta2 = r_6_eta2_1 + Intercept_eta2,
         .keep = "unused") %>%
  left_join(studynames,
            by = "study") %>%
  mutate(Decline = exp(eta1) / (1 + exp(eta1) + exp(eta2)),
         Adopt = exp(eta2) / (1 + exp(eta1) + exp(eta2)),
         Compromise = 1/(1 + exp(eta1) + exp(eta2))) %>% 
  pivot_longer(c(Decline, Adopt, Compromise),
               names_to = "par", values_to = "prob") %>%
  mutate(Parameter = factor(par, levels = c("Decline", "Adopt", "Compromise"))) %>%
  {ggplot(., aes(x = prob, y = studyname, color = fct_inorder(par), fill = fct_inorder(par))) +
      geom_boxplot(width = .3, linewidth = 1.2, outliers = F, position = position_dodge(.5)) +
      stat_summary(geom = "point", fun = mean,
                   color = rep(c("#80A4C2", "#E0BAD7", "#FFE3B3"), times = 147 / 3),
                   position = position_dodge(.5), size = 1.5, shape = 16) +
      scale_y_discrete(NULL) +
      scale_x_continuous(NULL, breaks = c(0, .25, .5, .75, 1),
                         labels = scales::percent_format()) +
      scale_color_manual(NULL, values = c("#003f5c", "#bc5090", "#ffa600")) +
      scale_fill_manual(NULL, values = c("#003f5c", "#bc5090", "#ffa600")) +
      coord_cartesian(xlim = c(0, 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            legend.position = "none")}

p_DAC_forestplt_legend <- (data.frame(par = factor(c(1, 2, 3), labels = c("Decline", "Adopt", "Compromise")),
                                      y = c(1, 2, 3)) %>%
                             ggplot(aes(x = par, y = y, color = par)) +
                             geom_point() +
                             scale_color_manual(NULL, values = c("#003f5c", "#bc5090", "#ffa600")) +
                             guides(color = guide_legend(override.aes = list(size = 4))) +
                             theme_minimal(base_size = 15) +
                             theme(legend.position = "top",
                                   legend.margin = margin(t = -10, unit = "pt")) )%>%
  cowplot::get_plot_component('guide-box-top', return_all = TRUE)

saveRDS(p_DAC_forestplt, here::here("Figures", "Metaanalysis", "p_DAC_forestplt.rds"))
saveRDS(p_DAC_forestplt_legend, here::here("Figures", "Metaanalysis", "p_DAC_forestplt_legend.rds"))


# study level intercepts 
stdylvl_dat <- data.frame(par = 1:nrow(data.frame(summary(helmer_stan, pars = "r_5_eta1_1")$summary)),
                          eta1 = data.frame(summary(helmer_stan, pars = "r_5_eta1_1")$summary)$mean +
                            data.frame(summary(helmer_stan, pars = "Intercept_eta1")$summary)$mean,
                          eta2 = data.frame(summary(helmer_stan, pars = "r_6_eta2_1")$summary)$mean +
                            data.frame(summary(helmer_stan, pars = "Intercept_eta2")$summary)$mean) %>% 
  mutate(Decline = exp(eta1)/(1 + exp(eta1) + exp(eta2)),
         Adopt = exp(eta2)/(1 + exp(eta1) + exp(eta2)),
         Compromise = 1/(1 + exp(eta1) + exp(eta2))) %>% 
  select(par, Decline, Adopt, Compromise) %>%
  pivot_longer(c("Decline", "Adopt", "Compromise"),
               names_to = "Parameter", values_to = "P") %>%
  mutate(Parameter = factor(Parameter, levels = c("Decline", "Adopt", "Compromise")))
saveRDS(stdylvl_dat, here::here("..", "Data Clean", "stdylvl_dat.rds"))

studylvl_plt <- ggplot(stdylvl_dat, aes(x = Parameter, y = round(P, 2), fill = Parameter, color = Parameter)) + 
  geom_violin(color = NA, alpha = .6) + 
  geom_jitter(shape = 16, alpha = .8, width = .2) +
  theme_minimal() +
  xlab("Intercept") +
  ylab("Probability") +
  guides(fill = "none") +
  scale_fill_manual(values = c("#003f5c", "#bc5090", "#ffa600")) +
  scale_color_manual(values = c("#003f5c", "#bc5090", "#ffa600")) +
  coord_cartesian(ylim = c(0, 1), expand = 0) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

saveRDS(studylvl_plt, here::here("Figures", "Metaanalysis", "studylvl_plt.rds"))
