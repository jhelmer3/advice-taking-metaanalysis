here::here("..", "Data Clean", "tri-m_more-pred_dat_2026-04-15.rds") |> 
  readRDS() |> summarize(.by = studyname, n_observations = n()) |> 
  gt() |>
  grand_summary_rows(
  columns = c(n_observations),
  fns = list(
    sum = ~sum(.)
  )
)
