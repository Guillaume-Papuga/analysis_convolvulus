#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 11.data_variable_extraction.R
# Combine datasets and extract environmental data
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

#####
# 0. Load data
#####
u.occ = read.csv(here::here("data", "processed", "u.occ.csv"), # upload data
                 sep = ",", header = T, dec = ".", row.names = 1)

u.abs = read.csv(here::here("data", "processed", "u.abs.csv"), # upload data
                 sep = ",", header = T, dec = ".", row.names = 1)

#####
# 1. Build the complete table
#####
# Combine the two datasets
dtst = bind_rows(u.occ %>% dplyr::select(presence, x, y), 
                 u.abs %>% dplyr::select(presence, x, y)) 

# Extract data

# Save the dataset
write.csv(bg.points, 
          here::here ("data", "processed", "complete_dataset.csv"))
