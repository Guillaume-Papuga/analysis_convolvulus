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


library(pegas)          # analyses génétiques complémentaires
library(geosphere)      # calculs géographiques
library(ape)  



## Packages conflict

# test github
