# Checking to see differences Shannon Diversity Indices between birds close & 
# far from the trail

# Visualizing
library(ggplot2)
ggplot(diversityclosenfar) + aes(x = dist, y = div) +
  geom_boxplot(fill = "#76D7FF") + labs(x = "Distance from Trail",
  y = "Shannon Diversity Index") +theme_classic()

# Try a Paired t-test 
library(dplyr)

# Test assumptions - normality
diff <- with(diversityclosenfar, div[dist == "close"] - div[dist == "far"])
shapiro.test(diff)

# p value is 0.09416, so that's greater than 0.05 and we can assume normality

# subset data to make it easier
close <- subset(diversityclosenfar, dist == "close", div, drop = TRUE)
far <- subset(diversityclosenfar, dist == "far", div, drop = TRUE)

# t test itself 
testing <- t.test(close, far, paired = TRUE)
testing



# try with SP RICH
ggplot(sprichclosenfar) +
  aes(x = dist, y = sprich) +
  geom_boxplot(fill = "#76D7FF") +
  labs(x = "Distance", y = "Species Richness") +
  theme_classic()

diff2 <- with(sprichclosenfar, sprich[dist == "close"] - sprich[dist == "far"])
shapiro.test(diff2)

# normal so we are good

close2 <- subset(sprichclosenfar, dist == "close", sprich, drop = TRUE)
far2 <- subset(sprichclosenfar, dist == "far", sprich, drop = TRUE)

testing2 <- t.test(close, far, paired = TRUE)
testing2
