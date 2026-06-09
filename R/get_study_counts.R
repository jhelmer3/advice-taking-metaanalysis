
get_study_counts <- function(full_dat, cleaned_dat) {
  rbind(
    cleaned_dat |>
      summarize(version = "no missing",
                studies = length(unique(study)),
                observations = n()),
    full_dat |>
      summarize(version = "full",
                studies = length(unique(study)),
                observations = n())
  )
}


