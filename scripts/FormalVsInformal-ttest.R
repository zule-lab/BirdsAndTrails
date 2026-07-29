# PAIRED t test between formal and informal sites
# Both NOT significant, but not by much.

library(dplyr)

# Read in data
t_testdata <- read.csv("t_testdata.csv")

# Test assumptions - normality
diff <- with(t_testdata, TotalDiv[ State == "Managed"] - TotalDiv[ State == "Un"])
shapiro.test(diff)

# p value is 0.2182, so that's greater than 0.05 and we can assume normality

# t test itself 
# for sp rich
testing <- t.test(TotalBirdR ~ State, data = t_testdata, paired = TRUE)
testing

# P value is 0.0719 

# t test for bird div

testing2 <- t.test(TotalDiv ~ State, data = t_testdata, paired = TRUE)
testing2

# P value is 0.08 

##
# Read in data 
closefarsprich <- read.csv("sprichclosenfar.csv")

testing3 <- t.test(sprich ~ dist, data = closefarsprich, paired = TRUE)

closefardiv <- read.csv("diversityclosenfar.csv")
testing4 <- t.test(div ~ dist, data = closefardiv, paired = TRUE)


