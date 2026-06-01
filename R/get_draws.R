
get_draws <- function(model) {
  model$draws(format = "df") |>
    as_tibble()
}

