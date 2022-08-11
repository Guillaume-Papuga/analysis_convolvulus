#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 04.convert_data_presence.R
# Transform the basic data into a spatial object 
# that fit the project options
# Authors : Guillaume Papuga
# Last update : 8 august 2022
#######################################################

#####
# 0. Load data
#####
d.occ = read.csv(here::here ("data", "processed", "d.occ.csv"))

#####
# 1. Filter
#####
# with ref.tile
ref.tile

# Delete multiple points wthin cells
d.occ = d.occ %>%
  mutate (cell = raster::cellFromXY(ref.tile, d.occ[, c("x", "y")])) %>%
  distinct(cell, .keep_all= TRUE) %>% # eliminate potential duplicate from different databases
  dplyr::select(-cell)

# Save the dataset
write.csv(u.occ, 
          here::here ("data", "processed", "u.occ.csv"))

# Restrain the dataset to the extent of the project

#####
# 2. Convert
#####

