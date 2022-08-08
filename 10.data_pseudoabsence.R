#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 10.data_pseudoabsence.R
# Create a raw set of many pseudo-absence
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

#####
# 3. Define the study area
#####


# Create a background area and sample bg points
d.occ_sf = st_as_sf (d.occ, coords = c("x", "y")) # convert to an SF object
# TD : change the CRS of the map to get the precise buffer size
bg.area = st_union(st_buffer(d.occ_sf, 1.5)) # draw a buffer of 
bg.points_sf = st_coordinates(st_sample(bg.area, 200)) # sample 200 points
bg.points = as.data.frame (bg.points_sf) %>%
  mutate (cell = cellFromXY(cur.st$wc2.1_10m_bio_1, bg.points_sf)) %>% # extract the cell number
  distinct(cell, .keep_all = TRUE) %>% # eliminate duplicates
  anti_join (d.occ) %>% # and delete those of d.occ
  rename(x = X, y = Y) 

# select bg.points that fall on land
bg.land = is.na(raster::extract(cur.st$wc2.1_10m_bio_1, bg.points [,c("x", "y")])) # create a vector TF
bg.points = bg.points %>%
  mutate (land = bg.land) %>% # add the vector
  filter (land == "FALSE") %>% # select the F (not in the sea)
  dplyr::select (-land) # delete the column
