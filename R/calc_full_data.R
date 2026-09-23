calc_full_data <- function(bird_div, quadrats_raw, activity_raw) {

	# calculate human activity
	activity <- activity_raw %>%
    select(c(Trail_ID, ID_Code, People, Effort)) %>%
    mutate(activity = (People / Effort) * 60) %>%
    group_by(Trail_ID) %>%
    summarize(avg_activity = mean(activity))

	# calculate stem density
	dens <- quadrats_raw %>%
		filter(Tally > 0) %>%
		select(c(ID, Tally, DBH_Class, distance)) %>%
		mutate(DBH_Calc = as.numeric(replace_string(DBH_Class)),
					 basal_quad_cat = (pi*(DBH_Calc^2)/40000)*Tally) %>%
		separate_wider_delim(ID, delim = "_", names = c("site", "transect", "gentry", "quadrat") ) %>%
		mutate(distance = case_when(quadrat == 1 ~ "far",
																quadrat == 2 ~ "mid",
																quadrat == 3 ~ "close"),
					 distance = case_when(distance == "close" ~ "close",
																distance == "mid" ~ "far",
																distance == "far" ~ "far")) %>%
		group_by(site, transect, distance) %>%
		summarize(stem_dens = sum(Tally),
							basal = sum(basal_quad_cat)) %>%
		mutate(Park = site,
					 # close distance is composed of 1 quadrat (4 m2)
					 # far distance is composed of 2 quadrats (8 m2)
					 stem_dens = case_when(distance == "close" ~ stem_dens/4,
					 											distance == "far" ~ stem_dens/8), # no. trees / m2
					 basal = case_when(distance == "close" ~ basal/0.0004,
					 									distance == "far" ~ basal/0.0008) # m2/ha
					 ) %>%
		unite("Trail_ID", c("site", "transect"), sep = "")

	bird_wide <- bird_div %>%
		pivot_wider(names_from = div, values_from = val)

	full <- inner_join(activity, dens) %>%
		full_join(., bird_wide, by = join_by(Trail_ID == site, distance == dist))

}



replace_string <- function(g) {
	sapply(g, function(a) {
		recode_values(a,
							 "< 1" ~ as.character(0.5),
							 "lessthan_ 1" ~ as.character(0.5),
							 "lessthan_1" ~ as.character(0.5),
							 "1_3" ~ as.character(2),
							 "3_5" ~ as.character(4),
							 "5_7" ~ as.character(6),
							 "7_10" ~ as.character(8.5),
							 "7_9" ~ as.character(8),
							 default = a
		)
	})
}
