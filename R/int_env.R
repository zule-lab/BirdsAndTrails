int_env <- function(ordination, full_data){

	# match datasets
	formal_env <- full_data %>%
		filter(form == "formal") %>%
		left_join(ordination$dat_form, ., by = c("Transect", "distance")) %>%
		select(c(avg_activity, stem_dens))

	informal_env <- full_data %>%
		filter(form == "informal") %>%
		left_join(ordination$dat_inform, ., by = c("Transect", "distance")) %>%
		select(c(avg_activity, stem_dens))

	# test env variables
	ord_form_env <- envfit(ordination$formal, formal_env, permutations = 999, na.rm = T)
	ord_inform_env <- envfit(ordination$informal, informal_env, permutations = 999, na.rm = T)

	env <- list(ord_form_env, ord_inform_env) %>% setNames(c('formal', 'informal'))
}
