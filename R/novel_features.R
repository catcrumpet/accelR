identify_bouts_pacat_ <- function(std_tb, epochlength, min_len = 0, max_len = Inf) {
  epochs_min <- 60L / epochlength

  # std_tb == tibble(timestamp, counts, pa, valid)
  std_tb %>%
    group_by(.bout_id = rleidv(pa), .pa = pa, .valid = valid) %>%
    summarise(.from = min(timestamp),
              .to = max(timestamp) + seconds(!!epochlength),
              .epochs = n(),
              .counts = sum(counts)) %>%
    ungroup() %>%
    mutate(.mins = .epochs / epochs_min) %>%
    select(-.bout_id)
}

identify_bouts_criterion_ <- function(std_tb, epochlength, pa_criterion) {
  epochs_min <- 60L / epochlength
  .pacrit <- enquo(pa_criterion)

  # std_tb == tibble(timestamp, counts, pa, valid)
  std_tb %>%
    mutate(.pa = !!.pacrit) %>%
    rename(.valid = valid) %>%
    group_by(.bout_id = rleidv(.pa), .pa, .valid) %>%
    summarise(.from = min(timestamp),
              .to = max(timestamp) + seconds(!!epochlength),
              .epochs = n(),
              .counts = sum(counts)) %>%
    ungroup() %>%
    mutate(.mins = .data$.epochs / epochs_min,
           .pa_criterion = rlang::as_label(.data$.pacrit)) %>%
    select(.pa_criterion, everything()) %>%
    select(-.bout_id)
}

identify_bouts_criterion_breaks_ <- function(std_tb, epochlength, pa_criterion, max_break_len) {
  epochs_min <- 60L / epochlength
  # min_bout_len <- min_bout_len * epochs_min
  max_break_len <- max_break_len * epochs_min

  .pacrit <- enquo(pa_criterion)

  # std_tb == tibble(timestamp, counts, pa, valid)
  std_tb %>%
    mutate(.pa = !!.pacrit) %>%
    group_by(rleid = rleidv(.pa), .pa, valid) %>%
    summarise(.from = min(timestamp),
              .to = max(timestamp) + seconds(!!epochlength),
              .epochs = n(),
              .counts = sum(counts)) %>%
    ungroup() %>%
    mutate(.pa_rev =
             if_else(!(.pa & valid) &
                       lag(.pa & valid, default = FALSE) &
                       lead(.pa & valid, default = FALSE) &
                       .epochs <= max_break_len,
                     TRUE, .pa),
           .valid = .pa_rev | valid) %>%
    group_by(.bout_id = rleidv(.pa_rev), .pa_rev, .valid) %>%
    summarise(.from = min(.from),
              .to = max(.to),
              .epochs_total = sum(.epochs),
              .counts_total = sum(.counts),
              .mins = .epochs_total / epochs_min,
              .epochs_pa = sum(.epochs[.pa]),
              .counts_pa = sum(.counts[.pa]),
              .mins_pa = .epochs_pa / epochs_min) %>%
    ungroup() %>%
    rename(.pa = .pa_rev, .epochs = .epochs_total, .counts = .counts_total) %>%
    mutate(.pa_criterion = rlang::as_label(.pacrit)) %>%
    select(.pa_criterion, everything()) %>%
    select(-.bout_id)
}

summarise_bouts_criteria_ <- function(bouts_data, ..., prefilter = TRUE) {
  .prefilt <- enquo(prefilter)
  .btsb <- filter(bouts_data, !!.prefilt)
  .crits <- enquos(...)

  tibble(.prefilter = rlang::as_label(.prefilt),
         .ovrl_mins_sum = sum(.btsb$.mins),
         .ovrl_mins_avg = mean(.btsb$.mins),
         .ovrl_cnts = nrow(.btsb),
         .ovrl_alpha = calculate_alpha_(.btsb$.mins),
         .ovrl_gini = reldist::gini(.btsb$.mins),
         .criterion = map_chr(.crits, rlang::as_label)) %>%
    mutate(.crit_mins_sum = map_dbl(.crits, ~sum(.btsb$.mins[rlang::eval_tidy(.x, .btsb)]))) %>%
    mutate(.crit_mins_avg = map_dbl(.crits, ~mean(.btsb$.mins[rlang::eval_tidy(.x, .btsb)]))) %>%
    mutate(.crit_cnts = map_dbl(.crits, ~sum(rlang::eval_tidy(.x, .btsb)))) %>%
    mutate(.crit_cnts_rat = .crit_cnts / .ovrl_cnts)
}

summarise_bouts_pctls_ <- function(bouts_data, percentiles, prefilter = TRUE) {
  .prefilt <- enquo(prefilter)
  .btsb <- filter(bouts_data, !!.prefilt)

  .pctls <- percentiles

  .quantiles <- quantile(.btsb$.mins, probs = .pctls)

  tibble(.prefilter = rlang::as_label(.prefilt),
         .pctl = names(.quantiles),
         .pctl_num = .pctls,
         .pctl_mins = .quantiles)
}

summarise_bouts_ <- function(bouts_data, lens, pctls, prefilter = TRUE) {
  .prefilt <- enquo(prefilter)
  .btsb <- filter(bouts_data, !!.prefilt)

  total_minutes <-
    tibble(lengths = lens) %>%
    mutate(total_minutes =
             map_dbl(lengths,
                     ~sum(filter(.btsb, .mins > .x)$.mins))) %>%
    mutate(lengths = str_c("mins_sum_gt", str_pad_0_adj(lengths))) %>%
    tidyr::spread(lengths, total_minutes) %>%
    mutate(mins_avg = mean(.btsb$.mins))

  count_ratio <-
    tibble(lengths = lens) %>%
    mutate(ratio =
             map_dbl(lengths,
                     ~nrow(filter(.btsb, .mins > .x))) %>%
             {. / nrow(.btsb)}) %>%
    mutate(lengths = str_c("cnts_rat_gt", str_pad_0_adj(lengths))) %>%
    tidyr::spread(lengths, ratio) %>%
    mutate(cnts_tot = nrow(.btsb)) %>%
    select(cnts_tot, everything())

  duration_percentiles <-
    quantile(.btsb$.mins, probs = pctls, names = FALSE) %>%
    enframe(name = "percentile", value = "minutes") %>%
    mutate(percentile = as.integer(pctls * 100)) %>%
    mutate(percentile = str_c("mins_len_p", str_pad_0_adj(percentile))) %>%
    tidyr::spread(percentile, minutes)

  bind_cols(total_minutes, count_ratio, duration_percentiles) %>%
    mutate(alpha = calculate_alpha_(.btsb$.mins)) %>%
    mutate(gini = reldist::gini(.btsb$.mins)) %>%
    mutate(.prefilter = rlang::as_label(.prefilt)) %>%
    select(.prefilter, everything())
}
