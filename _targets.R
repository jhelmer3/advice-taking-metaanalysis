
library(targets)
library(tarchetypes)
library(crew)

tar_option_set(
  controller = crew_controller_local(workers = 4),
  packages = c("tidyverse", "bayesplot", "cmdstanr", "patchwork", "posterior"),
  format = "qs"
)

tar_source()

folder_name <- "Tri-Hurdle Metaanalysis"
file_prefix <- "tri-m_more-pred"

# End this file with a list of target objects.
list(
  tar_target(testdat_file,
             here::here("..", "Data Clean", folder_name,
                        paste0(file_prefix, "_testdat.rds")),
             format = "file"),
  tar_target(dat_file, 
             here::here("..", "Data Clean", folder_name,
                        paste0(file_prefix, "_dat_2026-04-15.rds")),
             format = "file"),
  tar_target(model_file,
             here::here("Models", folder_name,
                        paste0(file_prefix, "_model.qs")),
             format = "file"),
  tar_target(testdat, readRDS(testdat_file)),
  tar_target(predictors, names(testdat) |> 
               purrr::discard(\(pred) stringr::str_detect(pred, "_by_"))),
  tar_target(dat, readRDS(dat_file)),
  tar_target(model, qs2::qs_read(model_file)),
  tar_target(draws, get_draws(model)),
  tar_target(draws_array, get_draws_array(model)),
  tar_target(rhats, get_rhats(draws)),
  tar_target(tracerank_plts, plt_n_tracerank(draws_array, rhats, 9)),
  tar_target(ppc_plt, plt_ppc(dat, draws)),
  tar_target(personlvl_dat, get_personlvl_dat(draws)),
  tar_target(itemlvl_dat, get_itemlvl_dat(draws)),
  tar_target(studylvl_dat, get_studylvl_dat(draws)),
  tar_target(itemlvl_plt, plt_intercepts(itemlvl_dat)),
  tar_target(personlvl_plt, plt_intercepts(personlvl_dat)),
  tar_target(studylvl_plt, plt_intercepts(studylvl_dat, plt_points = T)),
  tar_target(predtest_dat, make_predtest_dat(draws, testdat)),
  tar_target(patched_pred_plts,
             patch_pred_plts(predtest_dat, predictors),
             pattern = map(predictors),
             iteration = "list"),
  tar_quarto(report, 
             path = paste0("Reports/", file_prefix, "_report.qmd"),
             quiet = F)
)

