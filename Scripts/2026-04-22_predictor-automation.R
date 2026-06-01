woa_one_predictor_plt <- function(data, x) {

playdat <- data |>
  rename(condition_name = condition) |>
  inner_join(conditions |> 
               mutate(condition_levels = map(condition_levels, 
                                             \(condition_levels) 
                                             mutate(condition_levels, across(everything(), 
                                                                             \(var) case_when(
                                                                               str_detect(var, "\\.l") ~ "low", 
                                                                               str_detect(var, "\\.m") ~ "med", 
                                                                               str_detect(var, "\\.h") ~ "high")
                                             )))) |>
               mutate(condition_levels = map(condition_levels, 
                                             \(condition_levels) condition_levels |>
                                               filter(if_all(-x, ~ str_detect(.x, "med"))) |>
                                               select(x))) |>
               filter_out(map(condition_levels, nrow) == 0) |>
               unnest(condition_levels),
             by = "condition_name") |>
  select(-condition_name) |>
  mutate(!!sym(x) := factor(!!sym(x), levels = c("low", "med", "high"), ordered = T),
         woa = pmap_dbl(list(pc1, pc2, pc3, pc4, a, b), \(pc1, pc2, pc3, pc4, a, b) {
           data.frame(res = rmultinom(1, 1, c(pc1, pc2, pc3, pc4)),
                      woa = c(rbeta(1, a, b), 0, 1, 0.5)) |>
             filter(res == 1) |>
             pull(woa)}))

((playdat |>
    pivot_longer(c("pc1", "pc2", "pc3", "pc4"),
                 names_to = "choice", values_to = "prob") |>
    mutate(choice = factor(choice,
                           levels = c("pc2", "pc3", "pc4", "pc1"))) |> 
    ggplot(aes(y = prob, x = !!sym(x))) +
    geom_violin(alpha = .8) + 
    stat_summary(aes(x = !!sym(x)),
                 fun = "mean",
                 geom = "point",
                 position = position_dodge(0.9)) +
    scale_x_discrete(NULL) +
    scale_y_continuous("P", limits = c(0, 1)) +
    # scale_fill_manual(values = c("#ffd6ed", "#df94be", "#bc5090")) +
    # scale_color_manual(values = scales::col_darker(c("#ffd6ed", "#df94be", "#bc5090"))) +
    facet_wrap(~ choice, labeller = as_labeller(
      c(pc1 = "Compromise", pc2 = "Decline", pc3 = "Adopt", pc4 = "Midpoint")),
      nrow = 1) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "none",
          aspect.ratio = 1)) /
    (playdat |>
       ggplot(aes(x = !!sym(x), y = woa)) + 
       geom_violin(alpha = .8,
                   position = position_dodge(.9)) +  
       geom_point(data = playdat |>
                    # remove 0.5s just like remove decline and adopts?
                    filter(!(woa %in% c(0, 1))) |>
                    summarize(.by = c(!!sym(x)),
                              woa = mean(woa)),
                  aes(x = !!sym(x), y = woa),
                  position = position_dodge(.9),
                  size = 2) +
       stat_summary(aes(x = !!sym(x)),
                    fun = "mean",
                    geom = "point",
                    shape = 21,
                    fill = "white",
                    size = 2.5,
                    position = position_dodge(.9)) +
       scale_x_discrete(x |> gsub("_", " ", x =_) |> str_to_title(),
                        labels = c("-1 SD" = "No", "Mean" = "?", "+1 SD" = "Yes")) +
       labs(y = "WOA") +
       # scale_fill_manual(values = c("#ffd6ed", "#df94be", "#bc5090")) +
       # scale_color_manual(str_to_title(x[1]), 
       #                    values = scales::col_darker(c("#ffd6ed", "#df94be", "#bc5090"))) +
       theme_minimal(base_size = 12) +
       theme(panel.grid.minor = element_blank(),
             panel.grid.major.x = element_blank(),
             legend.position = "bottom")))
}