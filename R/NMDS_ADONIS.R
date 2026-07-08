# PERMANOVA and BETADISPER
library(tidyverse)
library(vegan)

# working with ABOVE25csv file 
# wide data frame for vegan

above25_tbl <- ABOVE25 %>% pivot_wider(names_from = "SPECIES", values_from="ABOVE25", values_fill=0)


above25_metadata <- above25_tbl %>% 
  select(TRAIL, SIZE)

above25_dist <- above25_tbl %>% select(-SIZE) %>% column_to_rownames("TRAIL") %>%
 avgdist(sample=13)

set.seed(1)
above25_nmds <- metaMDS(above25_dist) %>% scores() %>%
  as_tibble(rownames="TRAIL")

metadata_nmds <- inner_join(above25_metadata, above25_nmds)

metadata_nmds %>% ggplot(aes(x=NMDS1, y=NMDS2, color = SIZE)) + 
  geom_point()+
  stat_ellipse()

test <- adonis2(as.dist(above25_dist) ~ above25_metadata$SIZE, permutations = 1e6)
str(test)

bd <- betadisper(above25_dist, above25_metadata$SIZE)
anova(bd)

# Working with BELOW25 csv file 

below25_tbl <- BELOW25 %>% pivot_wider(names_from = "SPECIES", values_from = "BELOW25", values_fill = 0)

below25_metadata <- below25_tbl %>% select(TRAIL, SIZE)

below25_dist <- below25_tbl %>% select(-SIZE) %>% column_to_rownames("TRAIL") %>%
  avgdist(sample=7)

set.seed(1)
below25_nmds <- metaMDS(below25_dist) %>% scores() %>% 
  as_tibble(rownames = "TRAIL")

metadata_nmds2 <- inner_join(below25_metadata, below25_nmds)

metadata_nmds2 %>% ggplot(aes(x=NMDS1, y=NMDS2, color = SIZE)) + geom_point() + stat_ellipse()

test2 <- adonis2(as.dist(below25_dist) ~ below25_metadata$SIZE, permutations = 1e6)
str(test2)