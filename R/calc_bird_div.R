calc_bird_div <- function(birds_raw) {

  # collapse distance categories
  b_s <- birds_raw %>%
    rowwise() %>%
  	mutate(Transect = str_trim(Transect),
  				 X25. = sum(X25.50, X50., na.rm = T)) %>%
    select(c(Transect, Species, Date, X0.25, X25.))


  # filter for date with highest detection for each species
  b_close <- b_s %>%
    select(c(Transect, Species, X0.25)) %>%
    group_by(Transect, Species) %>%
    summarize(X0.25 = sum(X0.25)) %>%
  	pivot_wider(
      id_cols = Transect,
      names_from = Species,
      values_from = X0.25
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
    select(c(Transect, Species, X25.)) %>%
    group_by(Transect, Species) %>%
    summarize(X25. = sum(X25.)) %>%
    pivot_wider(
      id_cols = Transect,
      names_from = Species,
      values_from = X25.
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
    separate_wider_delim(dist, names = c('div', 'dist'), delim = '_') %>%
  	mutate(site = case_when(site == "ARBO-01" ~ "ARBOS1",
  													site == "ARBO-02" ~ "ARBOL1",
  													site == "ARBO-03" ~ "ARBOS2",
  													site == "ARBO-04" ~ "ARBOL2",
  													site == "BDL-1" ~ "BDLS1",
  													site == "BDL-2" ~ "BDLL1",
  													site == "BDL-3" ~ "BDLS2",
  													site == "BDL-4"~ "BDLL2",
  													site == "STNY-01" ~ "STNYL1",
  													site == "STNY-02" ~ "STNYL2",
  													site == "STNY-03" ~ "STNYS1",
  													site == "STNY-04" ~ "STNYS2",
  													site == "TECHNO-1" ~ "TECHL1",
  													site == "TECHNO-2" ~ "TECHS1",
  													site == "TECHNO-3" ~ "TECHL2",
  													site == "TECHNO-4" ~ "TECHS2"))
}
