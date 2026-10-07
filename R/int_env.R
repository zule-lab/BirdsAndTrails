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

	arboenv = ARBO_ORD[3:4]
	arboen = envfit(arbo.mds, arboenv, permutations = 999, na.rm = TRUE)
}
