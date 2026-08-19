calc_full_data <- function(bird_div, quadrats_raw, activity_raw) {

	# calculate human activity
	activity <- activity_raw %>%
    select(c(Trail_ID, ID_Code, People, Effort)) %>%
    mutate(activity = (People / Effort) * 60) %>%
    group_by(Trail_ID) %>%
    summarize(avg_activity = mean(activity))

	# calculate stem density
	dens <- quadrats_raw %>%
		select(c(ID, Tally, distance)) %>%
		group_by(ID, distance) %>%
		# quadrats are 4 m2
		summarize(stem_dens = sum(Tally)/4) %>%
		separate_wider_delim(ID, delim = "_", names = c("site", "transect", "gentry", "quadrat") ) %>%
		mutate(distance = case_when(quadrat == 1 ~ "far",
																quadrat == 2 ~ "mid",
																quadrat == 3 ~ "close"),

					 distance = case_when(distance == "close" ~ "close",
																distance == "mid" ~ "far",
																distance == "far" ~ "far")) %>%
		group_by(site, transect, distance) %>%
		summarize(stem_dens = mean(stem_dens, na.rm = T)) %>%
		mutate(Park = site) %>%
		unite("Trail_ID", c("site", "transect"), sep = "")

	bird_wide <- bird_div %>%
		pivot_wider(names_from = div, values_from = val)

	full <- inner_join(activity, dens) %>%
		full_join(., bird_wide, by = join_by(Trail_ID == site, distance == dist))

  }
