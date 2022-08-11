#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 10.data_pseudoabsence.R
# Create a raw set of many pseudo-absence
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

#####
# 0. Load data
#####
u.occ = read.csv(here::here("data", "processed", "u.occ.csv"), # upload data
                 sep = ",", header = T, dec = ".", row.names = 1)

#####
# 1. Create pseudo absence with SF
#####

# Convert the matrix to an SF object
u.occ_sf = st_as_sf (u.occ, coords = c("x", "y")) 

# Create a `background area` to sample the points
bg.area = st_union(st_buffer(u.occ_sf, 1.5)) # draw a buffer of 

# Sample the points
bg.points_sf = st_coordinates(st_sample(bg.area, 2000)) # sample 200 points

# Create a vector of `presence` cell number
psc.cell = cellFromXY(ref.tile, u.occ[,c("x", "y")])

# Filter and sort the DF
bg.points = as.data.frame (bg.points_sf) %>% # convert to a DF
  mutate (cell = cellFromXY(ref.tile, bg.points_sf)) %>% # extract the cell number
  distinct(cell, .keep_all = TRUE) %>% # eliminate duplicates i.e. points in the same cell
  filter (!cell %in% psc.cell) %>% # and delete those of u.occ
  rename (x = X, 
          y = Y)

# Select bg.points that fall on land
bg.land = is.na(raster::extract(cur_env$wc2.1_30s_elev, # create a vector T-F
                                bg.points [,c("x", "y")])) 

bg.points = bg.points %>%
  mutate (land = bg.land) %>% # add the vector
  filter (land == "FALSE") %>% # select the F (not in the sea)
  dplyr::select (-land, # delete two useless column
                 -cell) %>% 
  mutate (presence = 0) # Add the columns to fit the `occurence` format

# Save the dataset
write.csv(bg.points, 
          here::here ("data", "processed", "u.abs.csv"))
