
plt_n_trace <- function(draws_array, rhats, n = 9) {
  rhats |>
    head(n) |>
    pull(param) |>
    map(\(par) {
      slice <- draws_array[, , par, drop = FALSE] 
      bayesplot::mcmc_trace(slice, par) +
        labs(title = par) +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 12))
    }) |>
    wrap_plots(nrow = sqrt(n) |> floor())
}
