identify_bouts_pacat_ <- function(std_data, epochlength) {
  epochs_min <- 60L / epochlength

  std_data %>%
    dplyr::rename(.pa = pa, .valid = valid) %>%
    dplyr::mutate(.bout_id = data.table::rleidv(list(.pa, .valid))) %>%
    dplyr::group_by(.bout_id, .pa, .valid) %>%
    dplyr::summarise(.from = min(timestamp),
                     .to = max(timestamp) + lubridate::seconds(epochlength),
                     .epochs = dplyr::n(),
                     .counts = sum(counts)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.mins = .epochs / epochs_min) %>%
    dplyr::select(.bout_id:.epochs, .mins, .counts, dplyr::everything())
}

identify_bouts_criterion_ <- function(std_data, epochlength, pa_criterion) {
  epochs_min <- 60L / epochlength
  .pacrit <- rlang::enquo(pa_criterion)

  std_data %>%
    dplyr::mutate(.pa = !!.pacrit) %>%
    dplyr::select(timestamp, counts, .pa, .valid = valid) %>%
    dplyr::mutate(.bout_id = data.table::rleidv(list(.pa, .valid))) %>%
    dplyr::group_by(.bout_id, .pa, .valid) %>%
    dplyr::summarise(.from = min(timestamp),
                     .to = max(timestamp) + lubridate::seconds(epochlength),
                     .epochs = dplyr::n(),
                     .counts = sum(counts)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.mins = .epochs / epochs_min,
                  .pa_criterion = rlang::as_label(.pacrit)) %>%
    dplyr::select(.pa_criterion, .bout_id:.epochs, .mins, dplyr::everything())
}

identify_bouts_criterion_breaks_ <- function(std_data, epochlength, pa_criterion, max_break_mins) {
  epochs_min <- 60L / epochlength
  .pacrit <- rlang::enquo(pa_criterion)
  max_break_len <- max_break_mins * epochs_min

  std_data_rev <-
    std_data %>%
    dplyr::mutate(.pa = !!.pacrit) %>%
    dplyr::select(timestamp, counts, .pa, .valid = valid) %>%
    dplyr::mutate(.rleid = data.table::rleidv(list(.pa, .valid))) %>%
    dplyr::group_by(.rleid, .pa, .valid) %>%
    dplyr::summarise(.from = min(timestamp),
                     .to = max(timestamp) + lubridate::seconds(epochlength),
                     .epochs = dplyr::n(),
                     .counts = sum(counts)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.pa_rev =
                    dplyr::if_else(!(.pa & .valid) &
                                     dplyr::lag(.pa & .valid, default = FALSE) &
                                     dplyr::lead(.pa & .valid, default = FALSE) &
                                     .epochs <= max_break_len,
                                   TRUE, .pa),
                  .valid = .pa_rev | .valid)

  std_data_rev %>%
    dplyr::mutate(.bout_id = data.table::rleidv(list(.pa_rev, .valid))) %>%
    dplyr::group_by(.bout_id, .pa_rev, .valid) %>%
    dplyr::summarise(.from = min(.from),
                     .to = max(.to),
                     .epochs_total = sum(.epochs),
                     .counts_total = sum(.counts),
                     .epochs_pa = sum(.epochs[.pa]),
                     .counts_pa = sum(.counts[.pa])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.mins = .epochs_total / epochs_min,
                  .mins_pa = .epochs_pa / epochs_min) %>%
    dplyr::mutate(.pa_criterion = rlang::as_label(.pacrit)) %>%
    dplyr::select(.pa_criterion,
                  .bout_id,
                  .pa = .pa_rev,
                  .valid:.to,
                  .epochs = .epochs_total,
                  .mins,
                  .counts = .counts_total,
                  .epochs_pa,
                  .mins_pa,
                  .counts_pa,
                  dplyr::everything())
}

summarise_bouts_criteria_ <- function(bouts_tb, ..., prefilter = TRUE) {
  .prefilt <- rlang::enquo(prefilter)
  .btsb <- dplyr::filter(bouts_tb, !!.prefilt)
  .crits <- rlang::enquos(...)

  dplyr::tibble(.prefilter = rlang::as_label(.prefilt),
                .ovrl_mins_sum = sum(.btsb$.mins),
                .ovrl_mins_avg = mean(.btsb$.mins),
                .ovrl_cnts = nrow(.btsb),
                .ovrl_alpha = calculate_alpha_(.btsb$.mins),
                .ovrl_gini = reldist::gini(.btsb$.mins),
                .criterion = purrr::map_chr(.crits, rlang::as_label)) %>%
    dplyr::mutate(.crit_mins_sum =
                    purrr::map_dbl(.crits,
                                   ~sum(.btsb$.mins[rlang::eval_tidy(.x, .btsb)]))) %>%
    dplyr::mutate(.crit_mins_avg =
                    purrr::map_dbl(.crits,
                                   ~mean(.btsb$.mins[rlang::eval_tidy(.x, .btsb)]))) %>%
    dplyr::mutate(.crit_cnts =
                    purrr::map_dbl(.crits,
                                   ~sum(rlang::eval_tidy(.x, .btsb)))) %>%
    dplyr::mutate(.crit_cnts_rat = .crit_cnts / .ovrl_cnts)
}

summarise_bouts_pctls_ <- function(bouts_tb, percentiles, prefilter = TRUE) {
  .prefilt <- rlang::enquo(prefilter)
  .btsb <- dplyr::filter(bouts_tb, !!.prefilt)

  .pctls <- percentiles

  .quantiles <- stats::quantile(.btsb$.mins, probs = .pctls)

  dplyr::tibble(.prefilter = rlang::as_label(.prefilt),
                .pctl = names(.quantiles),
                .pctl_num = .pctls,
                .pctl_mins = .quantiles)
}

summarise_bouts_ <- function(bouts_tb, lengths, percentiles, prefilter = TRUE) {
  .prefilt <- rlang::enquo(prefilter)
  .btsb <- dplyr::filter(bouts_tb, !!.prefilt)

  total_minutes <-
    dplyr::tibble(lengths = !!lengths) %>%
    dplyr::mutate(total_minutes =
                    purrr::map_dbl(lengths,
                                   ~sum(dplyr::filter(.btsb, .mins > .x)$.mins))) %>%
    dplyr::mutate(lengths = stringr::str_c("mins_sum_gt", str_pad_0_adj(lengths))) %>%
    # replace with pivod_wider
    tidyr::spread(lengths, total_minutes) %>%
    dplyr::mutate(mins_avg = mean(.btsb$.mins))

  count_ratio <-
    dplyr::tibble(lengths = lengths) %>%
    dplyr::mutate(ratio =
                    purrr::map_dbl(lengths,
                                   ~nrow(dplyr::filter(.btsb, .mins > .x))) %>%
                    {. / nrow(.btsb)}) %>%
    dplyr::mutate(lengths = stringr::str_c("cnts_rat_gt", str_pad_0_adj(lengths))) %>%
    tidyr::spread(lengths, ratio) %>%
    dplyr::mutate(cnts_tot = nrow(.btsb)) %>%
    dplyr::select(cnts_tot, dplyr::everything())

  duration_percentiles <-
    stats::quantile(.btsb$.mins, probs = percentiles, names = FALSE) %>%
    tibble::enframe(name = "percentile", value = "minutes") %>%
    dplyr::mutate(percentile = as.integer(percentiles * 100)) %>%
    dplyr::mutate(percentile =
                    stringr::str_c("mins_len_p", str_pad_0_adj(percentile))) %>%
    tidyr::spread(percentile, minutes)

  dplyr::bind_cols(total_minutes, count_ratio, duration_percentiles) %>%
    dplyr::mutate(alpha = calculate_alpha_(.btsb$.mins)) %>%
    dplyr::mutate(gini = reldist::gini(.btsb$.mins)) %>%
    dplyr::mutate(.prefilter = rlang::as_label(.prefilt)) %>%
    dplyr::select(.prefilter, dplyr::everything())
}
