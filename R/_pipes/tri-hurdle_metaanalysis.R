
trihurdle_metaanalysis <- list(
  ## specifying files
  tar_target(tri_m_folder_name, "tri-hurdle_metaanalysis"),
  tar_target(tri_m_more_pred_file_prefix, "tri-m_more-pred"),
  tar_target(tri_m_model_file, 
             here::here("models", tri_m_folder_name,
                        paste0(tri_m_more_pred_file_prefix, "_model.qs")),
             format = "file"),
  
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
             quiet = F)
)