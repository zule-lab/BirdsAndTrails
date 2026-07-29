calc_bird_raw <- function(birds_raw){

	b_s <- birds_raw %>% select(c(Transect, Species, X0_25, X25_50, X50_)) %>%
		drop_na() %>%
		mutate(X25_ = X25_50 + X50_)



}
