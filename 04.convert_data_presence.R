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
u.occ = d.occ %>%
  mutate (cell = raster::cellFromXY(ref.tile, d.occ[, c("x", "y")])) %>%
  distinct(cell, .keep_all= TRUE) %>% # eliminate potential duplicate from different databases
  dplyr::select(-cell, -X)

# Save the dataset
write.csv(u.occ, 
          here::here ("data", "processed", "u.occ.csv"))

# Restrain the dataset to the extent of the project

#####
# 3. Save a summary table
#####

# Build the table
synth.tab = data.frame(t(table(u.occ$source))) #%>%
  dplyr::select(Var2, Freq) %>%
  dplyr::rename ("Source" = "Var2", # rename the variables
                 "N_filtered" = "Freq") 

# Write the table
write.csv(synth.tab, 
          here::here ("outputs", "tables", "synth.tab.unique.csv"))

#####
# 3. Convert
#####

