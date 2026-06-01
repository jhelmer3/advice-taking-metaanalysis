
get_rhats <- function(draws) {
  draws |>
    select(-`.chain`, -`.draw`) |>
    imap(\(ests, idx) tibble(param = idx,
                             rhat = posterior::rhat(ests))) |>
    list_rbind() |>
    arrange(desc(rhat))
}
