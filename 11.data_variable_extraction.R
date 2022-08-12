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
dt = bind_rows(u.occ %>% dplyr::select(presence, x, y), 
               u.abs %>% dplyr::select(presence, x, y)) 

# Extract data
env.tab = as.data.frame(raster::extract (cur_env, 
                                         dt %>% dplyr::select(x, y))) %>%
  mutate(presence = dt$presence) # add the presence column

# Rename columns
names(env.tab) = names(env.tab) %>%
  gsub("CHELSA.", "", .) %>%
  gsub("_1981.2010_V.2.1", "", .) %>%
  gsub("wc2.1_30s_elev", "elev", .)

# Save the dataset
write.csv(env.tab, 
          here::here ("data", "processed", "env.tab.csv"))
