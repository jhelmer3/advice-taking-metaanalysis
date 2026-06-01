
## setup ----

here::here()

library(bayesplot)
library(tidyverse)
library(patchwork)

options(scipen = 999)

## reading ----

dat <- readRDS(here::here("..", "Data Clean", "tri-m_small_more-pred_dat_2026-04-15.rds"))
conditions <- readRDS(here::here("..", "Data Clean", "more-pred_conditions.rds"))
condition_names <- readRDS(here::here("..", "Data Clean", "more-pred_condition-names.rds"))
fit <- qs2::qs_read(here::here("Models", "tri-m_small_more-pred_model_2.1.qs"))

predictors <- conditions |>
  select(-c(condition_id, condition_name)) |>
  head(1) |>
  unnest(condition_levels) |>
  names()

draws <- fit$draws(format = "draws_df") |>
  as_tibble()


# sampling diagnostics ---

fit$diagnostic_summary()

mcmc_rank_overlay(fit$draws(), c("Intercept_eta1", "Intercept_eta2", "Intercept_eta3"))
mcmc_rank_overlay(fit$draws(), c("mu", "phi"))

# posterior predictive ---

y_reps <- draws |>
  slice_sample(n = 50) |>
  select(starts_with("y_rep"))

ppcplt <- ppc_dens_overlay(y = dat$woa_winsor, yrep = as.matrix(y_reps)) +
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


ppcplt_g <- ppc_dens_overlay_grouped(y = dat$woa_winsor, yrep = as.matrix(y_reps), 
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

# forest plot

studylvl_dat <- draws |>
  select(c("r_5_eta1_1", "r_6_eta2_1", "r_9_eta3_1", 
           "Intercept_eta1", "Intercept_eta2", "Intercept_eta3") |>
           paste(collapse = "|") |>
           matches()) |>
  pivot_longer(-c(Intercept_eta1, Intercept_eta2, Intercept_eta3),
               names_to = "param_str", values_to = "est") |> 
  separate_wider_regex(param_str, 
                       patterns = c(par = "r_[569]_eta[123]_1", "\\[", study = "\\d+", "\\]")) |>
  mutate(.by = c(study, par),
         rep = row_number()) |>
  pivot_wider(id_cols = c("study", "rep", "Intercept_eta1", "Intercept_eta2", "Intercept_eta3"),
              names_from = par, values_from = est) |>
  mutate(eta1 = Intercept_eta1 + r_5_eta1_1,
         eta2 = Intercept_eta2 + r_6_eta2_1,
         eta3 = Intercept_eta3 + r_9_eta3_1,
         study = as.integer(study),
         .keep = "unused") |>
  left_join(dat |> select(study, studyname) |> unique(),
            by = "study") |>
  mutate(Decline = exp(eta1) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Adopt = exp(eta2) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Compromise = 1 / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
         Midpoint = exp(eta3) /  (1 + exp(eta1) + exp(eta2) + exp(eta3))) |> 
  pivot_longer(c(Decline, Adopt, Compromise, Midpoint),
               names_to = "par", values_to = "prob") |>
  mutate(Parameter = factor(par, levels = c("Decline", "Adopt", "Compromise", "Midpoint")))

p_DAC_forestplt <- studylvl_dat |>
  ggplot(aes(x = prob, y = studyname, color = fct_inorder(par), fill = fct_inorder(par))) +
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
            legend.position = "none")
p_DAC_forestplt

p_DAC_forestplt_legend <- (data.frame(par = factor(c(1, 2, 3, 4), labels = c("Decline", "Adopt", "Compromise", "Midpoint")),
                                      y = c(1, 2, 3, 4)) |>
                             ggplot(aes(x = par, y = y, color = par)) +
                             geom_point() +
                             scale_color_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
                             guides(color = guide_legend(override.aes = list(size = 4),
                                                         nrow = 2, byrow = T)) +
                             theme_minimal(base_size = 15) +
                             theme(legend.position = "top",
                                   legend.margin = margin(-10, 1, 1, 1, unit = "pt"))) |>
  cowplot::get_plot_component('guide-box-top', return_all = TRUE)

saveRDS(p_DAC_forestplt, here::here("Figures", "Tri-Hurdle Metaanalysis", "p_DAC_forestplt.rds"))
saveRDS(p_DAC_forestplt_legend, here::here("Figures", "Tri-Hurdle Metaanalysis", "p_DAC_forestplt_legend.rds"))

# study-level intercept

studylvl_plt <- studylvl_dat |>
  summarize(.by = c(study, Parameter),
            mean_prob = mean(prob),
            lower_prob = quantile(prob, (1 - .89) / 2),
            upper_prob = quantile(prob, .89 + (1 - .89) / 2)) |>
  ggplot(aes(x = Parameter, y = mean_prob, group = study, color = Parameter, fill = Parameter)) +
  geom_violin(aes(x = Parameter, y = mean_prob, fill = Parameter),
              inherit.aes = F,
              alpha = 0.4,
              color = NA) +
  geom_pointrange(aes(ymin = lower_prob, ymax = upper_prob),
                  position = position_jitter(0.15),
                  alpha = 0.8, shape = 16) +
  coord_cartesian(ylim = 0:1) +
  guides(x = guide_axis(cap = T)) +
  scale_x_discrete(NULL, expand = c(0, 0)) +
  scale_y_continuous(NULL, limits = 0:1, 
                     breaks = c(0, .25, .5, .75, 1),
                     labels = scales::percent_format()) +
  scale_color_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600") |> 
                       scales::col_lighter(amount = 5)) +
  scale_fill_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  theme_classic(base_size = 14) +
  theme(axis.line = element_blank(),
        axis.ticks= element_blank(),
        legend.position = "none")

saveRDS(studylvl_plt, here::here("Figures", "Tri-Hurdle Metaanalysis", "studylvl_plt.rds"))
