
plt_ppc <- function(dat, draws) {
  y <- dat$woa_winsor_trim
  # shortening y_rep just to match while data is misaligned
  # change when doing for real
  y_rep <- draws |> select(matches("y_rep")) |> head(50) |>
    as.matrix()
  
  # print(length(y))
  # print(ncol(y_rep))
  
  ppc_dens_overlay(y = y, yrep = y_rep) +
    theme(panel.background = element_blank(),
          legend.position = "bottom",
          text = element_text(family = "Roboto", size = 12),
          plot.margin = margin(2, 10, 2, 10),
          legend.margin = margin(-10, 2, 2, 2),
          axis.line.x = element_line())
}


  