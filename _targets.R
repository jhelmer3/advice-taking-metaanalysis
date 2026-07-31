
library(targets)
library(tarchetypes)
library(crew)

tar_option_set(
  controller = crew_controller_local(workers = 8),
  packages = c("tidyverse", "bayesplot", "cmdstanr", "patchwork", "posterior"),
  format = "qs"#,
  #storage = "worker",
  #retrieval = "worker"
)

tar_source()

folder_name <- "tri-hurdle_metaanalysis"
file_prefix <- "tri-m_more-pred"

# End this file with a list of target objects.
list(
  ## specifying files
  tar_target(full_dat_file, here::here("..", "woa_datasets.csv"),
             format = "file"),
  tar_target(testdat_file, here::here("..", "Data Clean", folder_name,
                        paste0(file_prefix, "_testdat.rds")),
             format = "file"),
  tar_target(dat_file, here::here("..", "Data Clean", folder_name,
                        paste0(file_prefix, "_dat_2026-04-15.rds")),
             format = "file"),
  tar_target(model_file, here::here("models", folder_name,
                        paste0(file_prefix, "_model.qs")),
             format = "file"),
  tar_target(full_dat, read.csv(full_dat_file)),
  
  ## cleaning data
  tar_target(cleaned_dat, clean_dat(full_dat)),
  tar_target(study_counts, get_study_counts(full_dat, cleaned_dat)),
  tar_target(testdat, readRDS(testdat_file)),
  tar_target(predictors, names(testdat) |> 
               purrr::discard(\(pred) stringr::str_detect(pred, "_by_"))),
  tar_target(dat, readRDS(dat_file)),
  
  ## model objects
  tar_target(model, qs2::qs_read(model_file)),
  tar_target(draws, get_draws(model)),
  tar_target(draws_array, get_draws_array(model)),
  
  ## model diagnostics
  tar_target(rhats, get_rhats(draws)),
  tar_target(tracerank_plts, plt_n_tracerank(draws_array, rhats, 9)),
  tar_target(trace_plts, plt_n_trace(draws_array, rhats, 9)),
  
  ## posterior predictive plots
  tar_target(ppc_plt, plt_ppc(dat, draws)),
  tar_target(ppc_plt_g, plt_ppc_g(dat, draws)),
  tar_target(ppc_plt_g_subsample, plt_ppc_g(dat, draws, 30)),

  ## intercept plots
  tar_target(personlvl_dat, get_personlvl_dat(draws)),
  tar_target(itemlvl_dat, get_itemlvl_dat(draws)),
  tar_target(studylvl_dat, get_studylvl_dat(draws)),
  
  tar_target(personlvl_plt_file, 
             ggsave_and_return_path("outputs/personlvl_plt.png",
                                    plt_intercepts(personlvl_dat), 
                                    width = 4, height = 2.5), format = "file"),
  tar_target(itemlvl_plt_file, 
             ggsave_and_return_path("outputs/itemlvl_plt.png",
                                    plt_intercepts(itemlvl_dat), 
                                    width = 4, height = 2.5), format = "file"),
  tar_target(studylvl_plt_file, 
             ggsave_and_return_path("outputs/studylvl_plt.png",
                                    plt_intercepts(studylvl_dat), 
                                    width = 4, height = 2.5), format = "file"),
  
  ## predictor plots
  tar_target(predtest_dat, make_predtest_dat(draws, testdat)),
  tar_target(patched_pred_plts,
             patch_pred_plts(predtest_dat, predictors),
             pattern = map(predictors),
             iteration = "list"),
  tar_target(patched_pred_plt_files,
             paste0("outputs/patched_pred_plt_", 
                    targets::tar_name(), ".png") |>
               ggsave_and_return_path(patched_pred_plts, 
                                      width = 6, height = 5.5),
             pattern = map(patched_pred_plts),
             iteration = "list",
             format = "file"),
  
  ## reports
  tar_quarto(report, 
             path = paste0("reports/", file_prefix, "_report.qmd"),
             quiet = F),
  tar_quarto(presentation_m3, 
             path = "presentations/M3_2026.qmd",
             quiet = F)
)

