calc_full_data <- function(bird_div, quadrats_raw, activity_raw) {
  activity <- activity_raw %>%
    select(c(Trail_ID, ID_Code, People, Effort)) %>%
    mutate(activity = (People / Effort) * 60) %>%
    group_by(Trail_ID) %>%
    summarize(avg_activity = mean(activity))

  stem_dens <-

  }
