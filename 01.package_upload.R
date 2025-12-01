#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 01.package_upload.R
# Upload packages
# Authors : Guillaume Papuga & Thomas Masclaux
# Last update : 5 novmber 2021
#######################################################

## R version
R.Version()

## Packages
library(ade4)
library(adegenet)
library(dplyr)
library(terra)
library(sf)
library(tidyr)
library(imager)
library(ggplot2)
library(reshape2)
library(hierfstat)
library(vegan)          # analyses multivariées (ex : mantel test)
library(maps)
library(pegas)          # analyses génétiques complémentaires
library(geosphere)      # calculs géographiques
library(ape)  
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(MASS) # pour kde2d
library(elevatr)
library(plotrix)
library(genepop)
library(sp) # ancien pour SF! A remplacer
library(grid)
library(ggmap)
library(poppr)
library(raster)

# Voir avec Eric!

#devtools::install_github('oswaldosantos/ggsn')

#library(diveRsity) # pas trouvé sur R CRAN
#library(ggsn) # pas trouvé
#library(LEA)

## Packages conflict

# test github
