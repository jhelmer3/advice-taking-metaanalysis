
clean_dat <- function(full_dat) {
  full_dat |>
    # removing gender because all rows that have a value for `female` also have a value for `gender`
    select(study, id, trial, 
           firstestimate, advice, woa_winsor, 
           female, age,
           student_judge, almanac, future, expert_advisor, incentive) |>
    # tossing trials where initial estimate was the same as the advice (gives WOA a denominator of zero)
    filter_out(firstestimate == advice) |>
    filter(between(age, 18, 100)) |>
    drop_na() |>
    mutate(distance = abs(firstestimate - advice) / firstestimate,
           woa_winsor_trim = case_when(woa_winsor == 1 ~ 0.999,
                                       woa_winsor == 0 ~ 0.001,
                                       .default = woa_winsor),
           DACM = recode_values(woa_winsor,
                                0 ~ 2,
                                1 ~ 3,
                                0.5 ~ 4,
                                default = 1)) |>
    # unfortunately tossing cases where first estimate was zero
    filter_out(distance == Inf) |>
    # giving everything a new ID (Stan needs consecutive increasing integers)
    mutate(.by = study,
           studyname = first(study),
           study = cur_group_id()) |>
    mutate(.by = c(study, id),
           id = cur_group_id()) |>
    mutate(.by = c(study, trial),
           trial = cur_group_id()) |>
    # standardizing distance within newly created trial (item) ID
    mutate(.by = trial,
           distance = (distance - mean(distance)) / sd(distance))
}