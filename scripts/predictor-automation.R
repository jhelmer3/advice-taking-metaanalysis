#predictor_labels <- list()

woa_predictor_plt <- function(data, x, colors = c("#b5cce0", "#6589a5", "#004c6d")) {
  
  condition_search_str <- str_split(condition_names[1], regex("\\.[lmh]{1}_")) |> 
    map(\(pred) 
        str_detect(pred, paste(x, collapse = "|")) |>
          ifelse(
            pred |>
              str_split_i("\\.", 1) |>
              paste0(... = _, ".[lmh]"),
            str_extract(pred, regex("^[a-z_]*")) |>
              paste0(... = _, 
                     ifelse(
                       str_detect(pred,
                                  "distance|confidence|age"),
                       ".m", ".l")))) |>
    pluck(1) |> paste(collapse = "_")
  
  playdat <- data %>%
    mutate(.by = condition,
           rep = row_number()) |>
    filter(str_detect(condition, condition_search_str)) %>%
    mutate(pred1 = condition |> str_extract(paste0(x[1], "\\.[lmh]{1}")) |> str_split_i("\\.", 2),
           pred2 = condition |> str_extract(paste0(x[2], "\\.[lmh]{1}")) |> str_split_i("\\.", 2),
           .keep = "unused",
           .before = everything()) |>
    mutate(across(starts_with("pred"), ~ factor(.x, levels = c("l", "m", "h"),
                                                labels = c("l" = "-1 SD", "m" = "Mean", "h" = "+1 SD"),
                                                ordered = T))) |>
    mutate(woa = pmap_dbl(list(pc1, pc2, pc3, pc4, a, b), \(pc1, pc2, pc3, pc4, a, b) {
      data.frame(res = rmultinom(1, 1, c(pc1, pc2, pc3, pc4)),
                 woa = c(rbeta(1, a, b), 0, 1, 0.5)) |>
        filter(res == 1) |>
        pull(woa)}))
  
  ((playdat |>
      pivot_longer(c("pc1", "pc2", "pc3", "pc4"),
                   names_to = "choice", values_to = "prob") %>%
      mutate(choice = factor(choice,
                             levels = c("pc2", "pc3", "pc4", "pc1"))) |> 
      ggplot(aes(y = prob, x = pred1, fill = pred2, color = pred2)) +
      geom_violin(alpha = .8) + 
      stat_summary(aes(x = pred1, group = pred2),
                   fun = "mean",
                   geom = "point",
                   position = position_dodge(0.9)) +
      scale_x_discrete(NULL) +
      scale_y_continuous("P",
                         limits = c(0, 1)) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = scales::col_darker(colors)) +
      facet_wrap(~ choice, labeller = as_labeller(
        c(pc1 = "Compromise", pc2 = "Decline", pc3 = "Adopt", pc4 = "Midpoint")),
        nrow = 1) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position = "none",
            aspect.ratio = 1)) /
      (playdat %>%
         ggplot(aes(x = pred1, y = woa, fill = pred2, color = pred2)) + 
         geom_violin(alpha = .8,
                     position = position_dodge(.9)) +  
         geom_point(data = playdat |>
                      # remove 0.5s just like remove decline and adopts?
                      filter(!(woa %in% c(0, 1))) |>
                      summarize(.by = c(pred1, pred2),
                                woa = mean(woa)),
                    aes(x = pred1, y = woa),
                    position = position_dodge(.9),
                    size = 2) +
         stat_summary(aes(x = pred1, group = pred2),
                      fun = "mean",
                      geom = "point",
                      shape = 21,
                      fill = "white",
                      size = 2.5,
                      position = position_dodge(.9)) +
         labs(fill = str_to_title(x[2]), x = str_to_title(x[1]), y = "WOA") +
         scale_fill_manual(values = colors) +
         scale_color_manual(str_to_title(x[2]), 
                            values = scales::col_darker(colors)) +
         theme_minimal(base_size = 12) +
         theme(panel.grid.minor = element_blank(),
               panel.grid.major.x = element_blank(),
               legend.position = "bottom")))
}

woa_one_predictor_plt <- function(data, x) {
  
  condition_search_str <- str_split(condition_names[1], regex("\\.[lmh]{1}_")) |> 
    map(\(pred) 
        str_detect(pred, paste(x, collapse = "|")) |>
          ifelse(
            pred |>
              str_split_i("\\.", 1) |>
              paste0(... = _, ".[lmh]"),
            str_extract(pred, regex("^[a-z_]*")) |>
              paste0(... = _, 
                     ifelse(
                       str_detect(pred,
                                  "distance|confidence|age"),
                       ".m", ".l")))) |>
    pluck(1) |> paste(collapse = "_")
  
  playdat <- data %>%
    mutate(.by = condition,
           rep = row_number()) |>
    filter(str_detect(condition, condition_search_str)) %>%
    mutate(pred1 = condition |> str_extract(paste0(x[1], "\\.[lmh]{1}")) |> str_split_i("\\.", 2),
           .keep = "unused",
           .before = everything()) |>
    mutate(across(starts_with("pred"), ~ factor(.x, levels = c("l", "h"),
                                                # changing! this now hard-coded to only work with
                                                # binary predictors. needs some love to work with both.
                                                labels = c("l" = "No", "h" = "Yes"),
                                                ordered = T))) |>
    mutate(woa = pmap_dbl(list(pc1, pc2, pc3, pc4, a, b), \(pc1, pc2, pc3, pc4, a, b) {
      data.frame(res = rmultinom(1, 1, c(pc1, pc2, pc3, pc4)),
                 woa = c(rbeta(1, a, b), 0, 1, 0.5)) |>
        filter(res == 1) |>
        pull(woa)}))
  
  ((playdat |>
      pivot_longer(c("pc1", "pc2", "pc3", "pc4"),
                   names_to = "choice", values_to = "prob") %>%
      mutate(choice = factor(choice,
                             levels = c("pc2", "pc3", "pc4", "pc1"))) |> 
      ggplot(aes(y = prob, x = pred1)) +
      geom_violin(alpha = .8) + 
      stat_summary(aes(x = pred1),
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
      (playdat %>%
         ggplot(aes(x = pred1, y = woa)) + 
         geom_violin(alpha = .8,
                     position = position_dodge(.9)) +  
         geom_point(data = playdat |>
                      # remove 0.5s just like remove decline and adopts?
                      filter(!(woa %in% c(0, 1))) |>
                      summarize(.by = c(pred1),
                                woa = mean(woa)),
                    aes(x = pred1, y = woa),
                    position = position_dodge(.9),
                    size = 2) +
         stat_summary(aes(x = pred1),
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

