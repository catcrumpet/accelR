identify_bouts_ <- function(std_data, epoch_len, min_len = 0, max_len = Inf) {
  # std_data == tibble(timestamp, counts, pa, valid)
  std_data %>%
    group_by(.bout_id = data.table::rleidv(pa), pa, valid) %>%
    summarise(.from = min(timestamp),
              .to = max(timestamp) + lubridate::seconds(!!epoch_len),
              .epochs = n(),
              .counts = sum(counts)) %>%
    ungroup() %>%
    mutate(.minutes = .epochs / (60L / !!epoch_len))
}

pa_bout_breaks_ <- function(std_data,
                            epoch_len,
                            pa_cat = c("mod", "vig"),
                            min_bout_len,
                            max_break_len) {
  epochs_min <- 60L / epoch_len

  min_bout_len <- min_bout_len * epochs_min
  max_break_len <- max_break_len * epochs_min

  std_data %>%
    transmute(timestamp,
              counts,
              pacat = (pa %in% pa_cat) & valid) %>%
    group_by(rleid = data.table::rleidv(pacat), pacat) %>%
    summarise(.from = min(timestamp),
              .to = max(timestamp) + lubridate::seconds(!!epoch_len),
              .epochs = n(),
              .counts = sum(counts)) %>%
    ungroup() %>%
    mutate(pacat_rev =
             if_else(!pacat &
                       lag(pacat, default = FALSE) &
                       lead(pacat, default = FALSE) &
                       .epochs <= max_break_len,
                     TRUE, pacat)) %>%
    group_by(.bout_id = data.table::rleidv(pacat_rev), pacat_rev) %>%
    summarise(.from = min(.from),
              .to = max(.to),
              .epochs_total = sum(.epochs),
              .epochs_pa = sum(.epochs[pacat]),
              .counts = sum(.counts)) %>%
    ungroup() %>%
    filter(.epochs_pa >= min_bout_len) %>%
    # filter(pacat_rev, .epochs_total >= min_bout_len) %>%
    select(-.bout_id, -pacat_rev) %>%
    mutate(.minutes = .epochs_total / (60L / !!epoch_len)) %>%
    mutate(.minutes_pa = .epochs_pa / (60L / !!epoch_len)) %>%
    rename(.epochs = .epochs_total)
}

summarise_bouts_ <- function(bouts_data,
                             pacat,
                             lengths,
                             percentiles) {
  bouts_subset <-
    bouts %>%
    filter(valid, pa == pacat)

  total_minutes <-
    tibble(lengths) %>%
    mutate(total_minutes =
             lengths %>%
             map_dbl(~sum(filter(bouts_subset, .minutes > .x)$.minutes))) %>%
    mutate(lengths = str_c("msum_gt",
                           str_pad(lengths, max(str_length(lengths)), "left", "0"))) %>%
    spread(lengths, total_minutes) %>%
    mutate(mavg = mean(bouts_subset$.minutes))

  count_ratio <-
    tibble(lengths) %>%
    mutate(ratio =
             lengths %>%
             map_dbl(~nrow(filter(bouts_subset, .minutes > .x))) %>%
             {. / nrow(bouts_subset)}) %>%
    mutate(lengths = str_c("crat_gt",
                           str_pad(lengths, max(str_length(lengths)), "left", "0"))) %>%
    spread(lengths, ratio) %>%
    mutate(ctot = nrow(bouts_subset)) %>%
    select(ctot, everything())

  duration_percentiles <-
    quantile(bouts_subset$.minutes, probs = percentiles, names = FALSE) %>%
    enframe(name = "percentile", value = "minutes") %>%
    mutate(percentile = percentiles) %>%
    mutate(percentile = str_c("mdur_pctl",
                              str_pad(as.integer(percentile * 100), 2, "left", "0"))) %>%
    spread(percentile, minutes)

  bind_cols(total_minutes, count_ratio, duration_percentiles) %>%
    mutate(alpha =
             bouts_subset$.minutes %>%
             {. / min(bouts_subset$.minutes)} %>%
             log() %>%
             mean(na.rm = TRUE) %>%
             {1 + (1 / .)}) %>%
    mutate(gini = reldist::gini(bouts_subset$.minutes))
}