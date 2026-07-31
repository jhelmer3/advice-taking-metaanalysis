
specify_general_data <- list(
  tar_target(full_dat_file, here::here("..", "woa_datasets.csv"),
             format = "file"),
  tar_target(testdat_file, here::here("..", "Data Clean", folder_name,
                                      paste0(file_prefix, "_testdat.rds")),
             format = "file"),
  tar_target(dat_file, here::here("..", "Data Clean", folder_name,
                                  paste0(file_prefix, "_dat_2026-04-15.rds")),
             format = "file"),
  tar_target(full_dat, read.csv(full_dat_file))
)