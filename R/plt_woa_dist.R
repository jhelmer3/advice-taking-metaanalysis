
plt_woa_dist <- function(fmtted_plt_dat, pred) {
  fmtted_plt_dat |>
    ggplot(aes(x = .data[[pred]], y = woa)) +
    geom_violin(alpha = .8,
                position = position_dodge(.9)) +
    geom_point(data = fmtted_plt_dat |>
                 # remove 0.5s just like remove decline and adopts?
                 filter_out(woa %in% c(0, 1)) |>
                 summarize(.by = all_of(pred),
                           woa = mean(woa)),
               aes(x = .data[[pred]], y = woa),
               position = position_dodge(.9),
               size = 2) +
    stat_summary(aes(x = .data[[pred]]),
                 fun = "mean",
                 geom = "point",
                 shape = 21,
                 fill = "white",
                 size = 2.5,
                 position = position_dodge(.9)) +
    labs(x = pred |> 
           str_replace_all("_|big5", " ") |>
           str_wrap(width = 15) |>
           str_to_sentence(), y = "WOA") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "bottom",
          axis.title.x = element_text(size = 18))
}

# fmt_plt_dat(tar_read(playdat), "big5_openness") |>
#   plt_woa_dist("big5_openness")

