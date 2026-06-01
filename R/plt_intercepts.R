
plt_intercepts <- function(lvl_dat, plt_points = F) {
  lvl_id <- lvl_dat |> select(ends_with("_id")) |> names()
  
  lvl_dat |>
    summarize(.by = c(all_of(lvl_id), Parameter),
              mean_prob = mean(prob),
              lower_prob = quantile(prob, (1 - .89) / 2),
              upper_prob = quantile(prob, .89 + (1 - .89) / 2)) |>
    ggplot(aes(x = Parameter, y = mean_prob, group = .data[[lvl_id]], color = Parameter, fill = Parameter)) +
    geom_violin(aes(x = Parameter, y = mean_prob, fill = Parameter),
                inherit.aes = F,
                alpha = 0.6,
                color = NA) +
    {if (plt_points) geom_jitter(alpha = 0.4, shape = 16, height = 0, width = .15)
      else NULL} +
    coord_cartesian(ylim = 0:1, clip = "off") +
    guides(x = guide_axis(cap = T),
           y = guide_axis(cap = T)) +
    scale_x_discrete(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL,  
                       breaks = c(0, .25, .5, .75, 1),
                       labels = scales::percent_format()) +
    scale_color_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600") |> 
                         scales::col_lighter(amount = 5)) +
    scale_fill_manual(NULL, values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
    labs(title = str_to_title(lvl_id) |> str_remove("_id")) +
    theme_classic(base_size = 14) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
}

# tar_read(personlvl_dat) |>
#   plt_intercepts()
# 
# tar_read(studylvl_dat) |>
#   plt_intercepts(plt_points = T)
