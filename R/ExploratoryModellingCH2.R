# Exploratory modelling 

library(lme4)
library(Matrix)
library(lmerTest)

mixed0 = lmer(TotalBirdR ~ TreesR + AvgPpl_H + (1|Site), data = datach2)
summary(mixed0)

mixed1 = lmer(BirdR_below ~ Near_quad + AvgPpl_H  + (1|Site), data = datach2)
summary(mixed1)

mixed2 = lmer(BirdR_above ~ Far_quad + TreesR  + (1|Site), data = datach2)
summary(mixed2)

mixed3 = lmer(BirdDiv_Below ~ Near_quad + TreesR + AvgPpl_H + Size + (1|Site), data = datach2)
summary(mixed3)

mixed4 = lmer(BirdDiv_Above ~ TreesR + AvgPpl_H + Size + (1|Site), data = datach2)
summary(mixed4)

mixed5 = lmer(TotalDiv ~ TreesR + AvgPpl_H + Size + (1|Site), data = datach2)
summary(mixed5)

interceptonly = lmer(TotalDiv ~ 1 + (1|Site), data = datach2)
summary(interceptonly)



