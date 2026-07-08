# Chapter 2 ordinations

# Load Packages

library(vegan)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)


# Read data on sp at each site 
ARBO_ORD <- read_csv("~/Desktop/CCs-Thesis/CC-bird-CH2/ARBO_ORD.csv")
View(ARBO_ORD)
BDL_ORD <-read_csv("~/Desktop/CCs-Thesis/CC-bird-CH2/BDL_ORD.csv")
View(BDL_ORD) 
STNY_ORD <- read_csv("~/Desktop/CCs-Thesis/CC-bird-CH2/STNY_ORD.csv")
View(STNY_ORD)
TECH_ORD <- read_csv("~/Desktop/CCs-Thesis/CC-bird-CH2/TECH_ORD.csv")
View(TECH_ORD)

# Read in data on informal vs formal
formal_ord <- read_csv("Formal.csv")
informal_ord <- read_csv("Informal.csv")


# Removing extra columns not needed
arbo_data <- ARBO_ORD[, -c(1:4)]
bdl_data <- BDL_ORD[, -c(1:4)]
stny_data <- STNY_ORD[, -c(1:4)]
tech_data <- TECH_ORD[, -c(1:4)]

# Removing columns for formal vs informal
formal_ord2 <- formal_ord[, -c(1:4)]
informal_ord2 <- informal_ord[, -c(1:4)]


# NMDS for each site
arbo.mds <- metaMDS(arbo_data, k =2)
bdl.mds <- metaMDS(bdl_data, k =2)
stny.mds <- metaMDS(stny_data, k =2)
tech.mds <- metaMDS(tech_data, k =2)

# NMDS for formal vs informal
formal.mds <- metaMDS(formal_ord2, k =2)
informal.mds <- metaMDS(informal_ord2, k =2)


# Adding in our environmental variables we want: Stem Density and People
arboenv = ARBO_ORD[3:4]
arboen = envfit(arbo.mds, arboenv, permutations = 999, na.rm = TRUE)
bdlenv = BDL_ORD[3:4]
bdlen = envfit(bdl.mds, bdlenv, permutations = 999, na.rm = TRUE)
stnyenv = STNY_ORD[3:4]
stnylen = envfit(stny.mds, stnyenv, permutations = 999, na.rm = TRUE)
techenv = TECH_ORD[3:4]
techen = envfit(tech.mds, techenv, permutations = 999, na.rm = TRUE)

# Environmental variables: Stem Density and People
informalenv = informal_ord[3:4]
informalen = envfit(informal.mds, informalenv, permutations = 999, na.rm = TRUE)
formalenv = formal_ord[3:4]
formalen = envfit(formal.mds, formalenv, permutations = 999, na.rm = TRUE)
summary(informalen)


# Final Group of Ordination Plots 

# Establishing layout for plots
layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE), heights = c(1, 1, 0.2))
par(mar = c(4, 4, 2, 1))

# Arboretum Ordination
ordi_arbo <- ordiplot(arbo.mds, type="n", xlab="NMDS Axis 1", ylab="NMDS Axis 2")
ordiellipse(arbo.mds, groups = ARBO_ORD$Dist, display= "sites", kind = "ehull", conf, label = F, draw = "polygon", col = c("lightblue", "orange"), alpha = 160)
orditorp(arbo.mds, display = "species", col = "black", air=4, pch =3)
plot(arboen, font = 2)
title(main = "Aboretum")

# Bois-de-Liesse Ordination
ordiplot(bdl.mds, type="n", xlab="NMDS Axis 1", ylab="NMDS Axis 2")
ordiellipse(bdl.mds, groups = BDL_ORD$Dist, display= "sites", kind = "ehull", conf, label = F, draw = "polygon", col = c("lightblue", "orange"), alpha = 160)
orditorp(bdl.mds, display="species", col="black", air=4, pch =3)
plot(bdlen, font = 2)
title(main = "Bois-de-Liesse")

# Stoneycroft Ordination
ordiplot(stny.mds, type="n", xlab="NMDS Axis 1", ylab="NMDS Axis 2")
ordiellipse(stny.mds, groups = STNY_ORD$Dist, display= "sites", kind = "ehull", conf, label = F, draw = "polygon", col = c("lightblue", "orange"), alpha = 160)
orditorp(stny.mds, display="species", col="black", air=4, pch=3)
plot(stnylen, font = 2)
title(main = "Stoneycroft")

# Technoparc Ordination
ordiplot(tech.mds, type="n", xlab="NMDS Axis 1", ylab="NMDS Axis 2")
ordiellipse(tech.mds, groups = TECH_ORD$Dist, display= "sites", kind = "ehull", conf, label = F, draw = "polygon", col = c("lightblue", "orange"), alpha = 160)
orditorp(tech.mds, display="species", col="black", air=4, pch = 3)
plot(techen, font = 2)
title(main = "Technoparc")

# Creating Legend
par(mar = c(0, 0, 0, 0))  # Adjust margins to minimize space around legend
plot.new()
legend("center", legend = c("Close", "Far"), col = "black", pt.bg = c("lightblue", "orange"), pch = 21, bty = "o", horiz = TRUE, cex = 1.2, pt.cex = 2.1)

##### FORMAL VS INFORMAL

par(mfrow=c(1, 1)) 
par(mar = c(5.1, 4.1, 4.1, 2.1))

# Formal Ordination
ordiplot(formal.mds, type = "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
ordiellipse(formal.mds, groups = formal_ord$Dist, display = "sites", kind = "ehull", conf, label = F, draw = "polygon", col = c("lightblue", "orange"), alpha = 160)
orditorp(formal.mds, display = "species", col = "black", air = 0.1, pch = 3)
title(main = "Formal Sites")

# Informal Ordination
ordiplot(informal.mds, type = "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
ordiellipse(informal.mds, groups = informal_ord$Dist, display = "sites", kind = "ehull", conf, label = F, draw = "polygon", col = c("lightblue", "orange"), alpha = 160)
orditorp(informal.mds, display = "species", col = "black", air = 0.1, pch = 3)
title(main = "Informal Sites")


# Add the shared legend to the right
plot.new()
legend("center", legend = c("Close", "Far"), fill = c("lightblue", "orange"), bty = "n")