
fmt_plt_dat <- function(predtest_dat, pred) {
  predtest_dat |>
    select(all_of(pred), draws) |>
    slice(.by = all_of(pred), 1) |>
    mutate(draws = map(
      draws, \(draws) draws |>
        mutate(woa = pmap_dbl(list(pc1, pc2, pc3, pc4, A, B), \(pc1, pc2, pc3, pc4, A, B) {
          data.frame(res = rmultinom(1, 1, c(pc1, pc2, pc3, pc4)),
                     woa = c(rbeta(1, A, B), 0, 1, 0.5)) |>
            filter(res == 1) |>
            pull(woa)})))) |>
    unnest(draws) |>
    pivot_longer(c("pc1", "pc2", "pc3", "pc4"),
                 names_to = "choice", values_to = "prob") |>
    mutate(choice = factor(choice,
                           levels = c("pc2", "pc3", "pc4", "pc1"))) |>
    filter_out(str_detect(pred, "female|age_by_female|student|almanac|future|expert|incentive") & 
                 .data[[pred]] == 0) |>
    mutate(across(all_of(pred), \(var) round(var, 2) |> factor()))
}

#fmt_plt_dat(tar_read(predtest_dat), "female")
