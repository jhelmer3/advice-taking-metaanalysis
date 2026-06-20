
plt_prob_by_choice <- function(fmtted_plt_dat, pred) {
  pred_levels <- fmtted_plt_dat |>
    select(all_of(pred)) |>
    unique() |>
    nrow()
  
  fmtted_plt_dat |>
    ggplot(aes(x = .data[[pred]], y = prob)) +
    geom_violin(alpha = .8) + 
    stat_summary(aes(x = .data[[pred]]),
                 fun = "mean",
                 geom = "point",
                 position = position_dodge(0.9)) +
    {if (pred_levels == 2) scale_x_discrete(NULL, labels = c("No", "Yes")) 
      else scale_x_discrete(NULL)} +
    scale_y_continuous("P",
                       limits = c(0, 1)) +
    facet_wrap(~ choice, labeller = as_labeller(
      c(pc1 = "Compromise", pc2 = "Decline", pc3 = "Adopt", pc4 = "Midpoint")),
      nrow = 1) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "none",
          aspect.ratio = 1)
}

# plt_dat <- tar_read(predtest_dat) |>
#   fmt_plt_dat("female")
# 
# 
# plt_prob_by_choice(plt_dat, "female")
# 
#   plt_prob_by_choice("female")
# fmt_plt_dat(tar_read(playdat), "big5_openness") |>
#   plt_prob_by_choice("big5_openness")

