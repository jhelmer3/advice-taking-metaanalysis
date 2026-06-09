
plt_ppc_g <- function(dat, draws) {
  y <- dat$woa_winsor_trim
  y_rep <- draws |> select(matches("y_rep")) |> head(50) |>
    as.matrix()
  
  ppc_dens_overlay_grouped(y = y, yrep = y_rep,
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
}
