
patch_pred_plts <- function(predtest_dat, pred) {
  fmtted_plt_dat <- fmt_plt_dat(predtest_dat, pred)
  
  prob_by_choice_plt <- plt_prob_by_choice(fmtted_plt_dat, pred)
  woa_dist_plt <- plt_woa_dist(fmtted_plt_dat, pred)
  
  wrap_plots(prob_by_choice_plt, woa_dist_plt, ncol = 1) 
}

#patch_pred_plts(tar_read(predtest_dat), tar_read(predictors)[[10]])