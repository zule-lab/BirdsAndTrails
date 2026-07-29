calc_bird_div <- function(birds_raw) {
  # collapse distance categories
  b_s <- birds_raw %>%
    select(c(Transect, Species, X0_25, X25_50, X50_)) %>%
    drop_na() %>%
    mutate(X25_ = X25_50 + X50_) %>%
    select(-c(X25_50, X50_))

  # calculate diversity metrics
  b_close <- b_s %>%
    select(c(Transect, Species, X0_25)) %>%
    group_by(Transect, Species) %>%
    summarize(X0_25 = sum(X0_25)) %>%
    pivot_wider(
      id_cols = Transect,
      names_from = Species,
      values_from = X0_25
    ) %>%
    replace(is.na(.), 0) %>%
    column_to_rownames('Transect') %>%
    select(-RWBL)

  div_close <- b_close %>%
    mutate(
      SR = specnumber(b_close),
      Shan = diversity(b_close, index = "shannon")
    ) %>%
    rownames_to_column('Transect')

  b_far <- b_s %>%
    select(c(Transect, Species, X25_)) %>%
    group_by(Transect, Species) %>%
    summarize(X25_ = sum(X25_)) %>%
    pivot_wider(
      id_cols = Transect,
      names_from = Species,
      values_from = X25_
    ) %>%
    replace(is.na(.), 0) %>%
    column_to_rownames('Transect') %>%
    select(-RWBL)

  div_far <- b_far %>%
    mutate(
      SR = specnumber(b_far),
      Shan = diversity(b_far, index = "shannon")
    ) %>%
    rownames_to_column('Transect')

  # make final dataset
  div <- full_join(
    div_close,
    div_far,
    by = "Transect",
    suffix = c("_close", "_far")
  ) %>%
    select(Transect, SR_close, SR_far, Shan_close, Shan_far) %>%
    mutate(
      site = str_extract(Transect, "^[^_]+"),
      form = case_when(
        str_detect(Transect, 'ARBO') == T ~ 'formal',
        str_detect(Transect, 'BDL') == T ~ 'formal',
        str_detect(Transect, 'STNY') == T ~ 'informal',
        str_detect(Transect, 'TECHNO') == T ~ 'informal'
      )
    ) %>%
    pivot_longer(
      cols = c(SR_close, SR_far, Shan_close, Shan_far),
      names_to = 'dist',
      values_to = 'val'
    ) %>%
    separate_wider_delim(dist, names = c('div', 'dist'), delim = '_')
}
