
# n_studies will need to be updated if ever full dataset has more than 59 studies
plt_ppc_g <- function(dat, draws, n_studies = 59) {
  y_rep <- draws |> select(matches("y_rep")) |> head(50) |>
    as.matrix()
  
  study_subsample <- dat |>
    mutate(idx = row_number()) |>
    filter(study %in% sample(unique(study), n_studies))
  
  # y_subsample and y_rep_subsample will contain the full data if
  # n_studies is not specified
  y_subsample <- study_subsample$woa_winsor_trim
  y_rep_subsample <- y_rep[, study_subsample |>
                             pull(idx) |>
                             paste0("y_rep[", ... = _, "]")]
  
  ppc_dens_overlay_grouped(y = y_subsample, yrep = y_rep_subsample,
                   group = study_subsample |>
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




