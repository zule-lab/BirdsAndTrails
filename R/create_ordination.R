create_ordination <- function(birds_raw){

	# NMDS is for when you want to represent the objects in a small number of axes
	# lower the stress value, the better the representation of objects in ordination space

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
		top_n(1, X0.25) %>%
		unique() %>%
		pivot_wider(
			id_cols = Transect,
			names_from = Species,
			values_from = X0.25
		) %>%
		replace(is.na(.), 0) %>%
		dplyr::select(-c('RWBL', 'RWBL ')) %>%
		mutate(formality = case_when(str_detect(Transect, 'ARBO') == T ~ 'formal',
																 str_detect(Transect, 'BDL') == T ~ 'formal',
																 str_detect(Transect, 'TECHNO') == T ~ 'informal',
																 str_detect(Transect, 'STNY') == T ~ 'informal'))


	b_far <- b_s %>%
		select(c(Transect, Species, X25.)) %>%
		group_by(Transect, Species) %>%
		top_n(1, X25.) %>%
		unique() %>%
		pivot_wider(
			id_cols = Transect,
			names_from = Species,
			values_from = X25.
		) %>%
		replace(is.na(.), 0) %>%
		dplyr::select(-c('RWBL', 'RWBL ')) %>%
		mutate(formality = case_when(str_detect(Transect, 'ARBO') == T ~ 'formal',
																 str_detect(Transect, 'BDL') == T ~ 'formal',
																 str_detect(Transect, 'TECHNO') == T ~ 'informal',
																 str_detect(Transect, 'STNY') == T ~ 'informal'))

	# separate by formality
	b_close_form <- b_close %>%
		ungroup() %>%
		filter(formality == 'formal') %>%
		select(-c(formality)) %>%
		mutate(distance = 'close')

	b_close_inform <- b_close %>%
		ungroup() %>%
		filter(formality == 'informal') %>%
		select(-c(formality)) %>%
		mutate(distance = 'close')

	b_far_form <- b_far %>%
		ungroup() %>%
		filter(formality == 'formal') %>%
		select(-c(formality)) %>%
		mutate(distance = 'far')

	b_far_inform <- b_far %>%
		ungroup() %>%
		filter(formality == 'informal') %>%
		select(-c(formality)) %>%
		mutate(distance = 'far')

	b_form <- bind_rows(b_close_form, b_far_form) %>%
		mutate(across(everything(), ~replace_na(.,0))) %>%
		select_if(~ !is.numeric(.) || sum(.) != 0)

	b_inform <- bind_rows(b_close_inform, b_far_inform) %>%
		mutate(across(everything(), ~replace_na(.,0))) %>%
		select_if(~ !is.numeric(.) || sum(.) != 0)

	# ordinate
	ord_form <- metaMDS(b_form %>% select(-c(distance, Transect)), distance = "bray", k = 2)
	ord_inform <- metaMDS(b_inform %>% select(-c(distance, Transect)), distance = "bray", k = 2)

	ords <- list(b_form, ord_form, b_inform, ord_inform) %>% setNames(c('dat_form', 'formal', 'dat_inform', 'informal'))


	}
