# Calculating Shannon Diversity 

# Convert zero-25 to wide-form data 
install.packages("vegan")
library(vegan)
library(tidyr)
library(dplyr)
ZEROTO25.wide <- spread(BELOW25, SPECIES, BELOW25, fill = 0)
ZEROTO25.wide

to25 = select(ZEROTO25.wide, -2)
to25
str(to25)

close <- to25[-1]

sp2_curve <- specaccum(to25_new, method = "random", permutations = 1000)
plot(sp2_curve)
sr <- specnumber(to25_new)
sr
mean(sr)
sd(sr)

# Shannon's Index (proportional abundance of sp in sample)
shannon1 <- diversity(to25_new, index = "shannon")
shannon1
# larger the number more diverse and even 
mean(shannon1)
sd(shannon1)

# Next looking at bird div further from trails
ABOVE25.wide <- spread(ABOVE25, SPECIES, ABOVE25, fill = 0)
ABOVE25.wide
up25 = select(ABOVE25.wide, -2)
up25
far <- up25[-1]

sp3_curve <-specaccum(up25_new, method = "random", permutations = 1000)
plot(sp3_curve)
sr2 <- specnumber(up25_new)
sr2

# etc etc 

# Shannon's Index
shannon2 <- diversity(up25_new, index = "shannon")
shannon2
sd(shannon2)

mean(shannon2)
sd(shannon2)


# standard deviation below 25 = 0.30379
sdb <-c(0.30379)

ggplot(b25) +
aes(x = TRAIL, y = SHAN_BELOW25) +
  geom_col(fill = "#46337E") +
  labs(x = "Trail", y = "Shannon Diversity Index") +
  theme_classic() + geom_errorbar(aes(ymin = SHAN_BELOW25-sdb, ymax = SHAN_BELOW25+sdb, width=0.2))


# standard deviation above 25 = 0.1673341
sda <- c(0.1673341)
ggplot(b25) +
  aes(x = TRAIL, y = SHAN_ABOVE25) +
  geom_col(fill = "#4682B4") +
  labs(x = "Trail", y = "Shannon Diversity Index") +
  theme_classic() + geom_errorbar(aes(ymin = SHAN_ABOVE25-sdb, ymax = SHAN_ABOVE25+sdb, width=0.2))


# Looking at total bird diversity 

totalbirds.wide <- spread(TotalBirdDiv, SPECIES, TOTAL, fill = 0)
totalbirds.wide

totaldiv = select(totalbirds.wide, -1)
totaldiv
str(totaldiv)

shannon3 <- diversity(totaldiv, index = "shannon")
shannon3

sp3_curve <- specaccum(totaldiv, method = "random", permutations = 1000)
plot(sp3_curve)
sr3 <- specnumber(totaldiv)
sr3
mean(sr3)
sd(sr3)


