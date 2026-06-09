library(tidyverse)

d <- targets::tar_read(dat) |>
  tibble()

d |>
  select(age, woa_winsor) |>
  mutate(age_z = (age - mean(age)) / sd(age),
         age_bin = case_when(
           between(age_z, -.75, -.25) ~ -1,
           between(age_z, -.25, .25) ~ 0,
           between(age_z, .25, .75) ~ 1,
         ),
         decline = ifelse(woa_winsor == 0, 1, 0),
         adopt = ifelse(woa_winsor == 1, 1, 0),
         compromise = ifelse(between(woa_winsor, .00001, .99999), 1, 0)) |>
  summarize(.by = age_bin,
            across(c(decline, adopt, compromise), mean)) |>
  pivot_longer(!age_bin,
               names_to = "choice", values_to = "prop") |>
  ggplot(aes(x = age_bin, y = prop)) +
  geom_point() +
  facet_wrap(~ choice)

d |>
  select(studyname, age, woa_winsor) |>
  mutate(age_z = (age - mean(age)) / sd(age),
         age_bin = case_when(
           between(age_z, -1.5, -.5) ~ -1,
           between(age_z, -.5, .5) ~ 0,
           between(age_z, .5, 1.5) ~ 1,
         ),
         decline = ifelse(woa_winsor == 0, 1, 0),
         adopt = ifelse(woa_winsor == 1, 1, 0),
         compromise = ifelse(between(woa_winsor, .00001, .99999), 1, 0)) |>
  summarize(.by = c(studyname, age_bin),
            across(c(decline, adopt, compromise), mean)) |>
  pivot_longer(c(decline, adopt, compromise),
               names_to = "choice", values_to = "prop") |>
  ggplot(aes(x = age_bin, y = prop, group = studyname)) +
  geom_line(alpha = .6) +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = \(x) purrr::map_chr(x, 
                                                  \(xi) paste0(rep("+", as.integer(abs(xi))), 
                                                               xi, " SD"))) +
  facet_wrap(~ choice) +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")

d |>
  select(studyname, female, woa_winsor) |>
  filter(studyname %in% sample(studyname, 15)) |>
  mutate(decline = ifelse(woa_winsor == 0, 1, 0),
         adopt = ifelse(woa_winsor == 1, 1, 0),
         compromise = ifelse(between(woa_winsor, .00001, .99999), 1, 0)) |>
  summarize(.by = female,
            across(c(decline, adopt, compromise), mean)) |>
  pivot_longer(!female,
               names_to = "choice", values_to = "prop") |>
  ggplot(aes(x = female, y = prop)) +
  geom_point() +
  facet_wrap(~ choice)+
  theme_linedraw() +
  theme(panel.grid.minor = element_blank())


d |>
  mutate(.by = studyname,
         sample_size = length(unique(id))) |>
  select(studyname, sample_size, female, woa_winsor) |>
  mutate(decline = ifelse(woa_winsor == 0, 1, 0),
         adopt = ifelse(woa_winsor == 1, 1, 0),
         compromise = ifelse(between(woa_winsor, .00001, .99999), 1, 0)) |>
  summarize(.by = c(female, studyname),
            across(c(decline, adopt, compromise), mean),
            sample_size = first(sample_size)) |>
  summarize(.by = c(studyname),
            across(c(decline, adopt, compromise), ~ reduce(.x, `-`)),
            sample_size = first(sample_size)) |>
  pivot_longer(!c(studyname, sample_size),
               names_to = "choice", values_to = "difference") |>
  ggplot(aes(y = studyname, x = difference, size = sample_size)) +
  annotate("segment", x = 0, y = -Inf, yend = Inf, linewidth = .1) +
  geom_vline(aes(xintercept = mean(difference)), color = "red") +
  geom_point() +
  facet_grid(~ choice) +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank())

m <- lme4::lmer(woa_winsor ~ age * female + (1 | trial) + (1 | id) + (1 | studyname), 
           data = d)
sjPlot::tab_model(m)
sjPlot::plot_model(m, type = "int")
