# Linear Modelling 


library(readr)

ch2_linear <- read_csv("ch2_linear.csv")
View(ch2_linear)

# Management style as a factor
ch2_linear$Management <- as.factor(ch2_linear$Management)
ch2_linear$Dist <- as.factor(ch2_linear$Dist)

# Model for Shannon Diversity 
birddiv = lm(ShanDiv ~ Management + Dist*AvgPpl + StemDensity, data = ch2_linear)
summary(birddiv)
par(mfrow = c(2,2))
plot(birddiv)


# Model for Species Richness

birdsprch = lm(SpRich ~ Management + Dist*AvgPpl + StemDensity, data = ch2_linear)
summary(birdsprch)
par(mfrow = c(2,2))

