
get_itemlvl_dat <- function(draws) {
  draws |>
    select(c("r_2_eta1_1", "r_4_eta2_1", "r_8_eta3_1", 
             "Intercept_eta1", "Intercept_eta2", "Intercept_eta3") |>
             paste(collapse = "|") |>
             matches()) |>
    pivot_longer(-c(Intercept_eta1, Intercept_eta2, Intercept_eta3),
                 names_to = "param_str", values_to = "est") |> 
    separate_wider_regex(param_str, 
                         patterns = c(par = "r_[248]_eta[123]_1", "\\[", item_id = "\\d+", "\\]")) |>
    mutate(.by = c(item_id, par),
           rep = row_number()) |>
    pivot_wider(id_cols = c("item_id", "rep", "Intercept_eta1", "Intercept_eta2", "Intercept_eta3"),
                names_from = par, values_from = est) |>
    mutate(eta1 = Intercept_eta1 + r_2_eta1_1,
           eta2 = Intercept_eta2 + r_4_eta2_1,
           eta3 = Intercept_eta3 + r_8_eta3_1,
           item_id = as.integer(item_id),
           .keep = "unused") |>
    mutate(Decline = exp(eta1) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
           Adopt = exp(eta2) / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
           Compromise = 1 / (1 + exp(eta1) + exp(eta2) + exp(eta3)),
           Midpoint = exp(eta3) /  (1 + exp(eta1) + exp(eta2) + exp(eta3))) |> 
    pivot_longer(c(Decline, Adopt, Compromise, Midpoint),
                 names_to = "par", values_to = "prob") |>
    mutate(Parameter = factor(par, levels = c("Decline", "Adopt", "Compromise", "Midpoint")))
}