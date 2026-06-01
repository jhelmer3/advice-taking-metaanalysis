
plt_n_tracerank <- function(draws_array, rhats, n = 9) {
  rhats |>
    head(n) |>
    pull(param) |>
    map(\(par) {
      slice <- draws_array[, , par, drop = FALSE] 
      bayesplot::mcmc_rank_overlay(slice, par) +
          labs(title = par) +
          theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 12))
      }) |>
    wrap_plots(nrow = sqrt(n) |> floor())
}

#bayesplot::mcmc_rank_overlay(tar_read(draws_array), "r_7_eta3_1[7172]")
# plt_n_tracerank(tar_read(draws_array), tar_read(rhats))
