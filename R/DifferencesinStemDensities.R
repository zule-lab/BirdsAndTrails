# PLOTTING STEM DENSITIES 

library(ggplot2)

far <- ch2data$Far_quad
mid <- ch2data$Mid_quad
close <- ch2data$Near_quad

plot(far)
boxplot(far) + boxplot(mid)
ggplot(densities) +
  aes(x = woodystems, y = density) +
  geom_boxplot(fill = "#A9C8FF") +
  theme_classic()