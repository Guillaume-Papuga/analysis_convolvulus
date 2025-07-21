#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 03.project_attributes.R
# Set the project attributes
# Authors : Guillaume Papuga
# Last update : 8 august 2022
#######################################################

#####
# 1. Define paths
#####
# Raster : define the path to the source folder
climate.folder = "/media/papuga/TOSHIBA EXT/02.spatial.data/20.environement/climat/chelsa_bioclim" # climate folder for the project
geo.folder = "/media/papuga/TOSHIBA EXT/02.spatial.data/10.geographie"

#####
# 2. Set spatial attributes
#####
# Set the basic parameters of the project
# each time you want to stack data, you have to respect the same PER : PROJECTION - EXTENT - RESOLUTION
# it's called the standard settings of the project.

## A. Upload a raster
ref.tile = raster (paste(climate.folder, "/", list.files(climate.folder)[1], sep = "")) # the first tile is the reference

# B. Project projection
p.proj = crs(ref.tile)

# C. Project extent
d.occ = read.csv(here::here ("data", "processed", "d.occ.csv"))
ext.matrix = matrix (nrow = 2, ncol = 2, 
                     byrow = T, 
                     data = c(min (d.occ$x) - abs(max (d.occ$x) - min (d.occ$x))*0.30,  # add a percentage of the real extent
                              max (d.occ$x) + abs(max (d.occ$x) - min (d.occ$x))*0.30, 
                              min (d.occ$y) - abs(max (d.occ$y) - min (d.occ$y))*0.40, 
                              max (d.occ$y) + abs(max (d.occ$y) - min (d.occ$y))*0.40))
p.extent = extent (ext.matrix)

# D. Project resolution 
p.res = res(ref.tile)

############################ IMPORT DATA ##################################################

#####
# 3. Current climate
#####

### A. Import data
# List the files in the folder
tiles = list.files(path = paste (climate.folder, "/current", sep = "")) # names of each tile

# Retain the `bio` variables
cur_tiles = grep("bio", tiles, value = TRUE)  

# load as a `stack`
cur.st = stack(paste(climate.folder, "/current/", cur_tiles, sep = ""))

### B. Transform data
# Project projection

# Project extent
# crop to the spatial extent
# lim = as(ext.stud, Class = "Spatial")
#cur.st.m = raster::mask(cur.st, lim)
cur.st.c = raster::crop (cur.st, p.extent)

# Project resolution 

#####
# 4. Past climate
#####

#####
# 5. Elevation
#####

### A. Import data
elevation = raster(paste(geo.folder, "/worldclim.elevation/wc2.1_30s_elev.tif", sep = ""))

### B. Transform data
# Project projection
crs(elevation)

# Project extent
elevation.c = raster::crop (elevation, p.extent)

# Project resolution 

#####
# 6. Assemble maps
#####

# Current
cur_env = stack (cur.st.c, 
                 elevation.c)

# Past


