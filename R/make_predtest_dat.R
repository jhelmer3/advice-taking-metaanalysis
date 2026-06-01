
make_predtest_dat <- function(draws, testdat) {
  draws |>
    posterior::as_draws_df() |>
    as_tibble() |>
    mutate(rep = row_number()) |>
    select(rep, starts_with("A_t"),
           starts_with("B_t"),
           starts_with("pc")) |>
    pivot_longer(
      !rep,
      names_to = c(".value", "condition_id"),
      names_pattern = "([ABpc\\d]+)_t\\[(\\d+)\\]"
    ) |>
    nest(draws = !condition_id) |>
    bind_cols(testdat)
}



