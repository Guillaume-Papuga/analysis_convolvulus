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
climate.folder = "/media/papuga/LaCie/01.spatial_data/climate/" # climate folder for the project
act.climate.folder= "/media/papuga/LaCie/01.spatial_data/climate/CHELSA_bioclim_actual/" # climate folder for the project
lgm.climate.folder = "/media/papuga/LaCie/01.spatial_data/climate/CHELSA_bioclim_past_LGM/" # climate folder for the project
mholo.climate.folder = "/media/papuga/LaCie/01.spatial_data/climate/CHELSA_bioclim_past_midholo/" # climate folder for the project
geo.folder = "/media/papuga/LaCie/01.spatial_data/geography/" 

#####
# 2. Set spatial attributes
#####
# Set the basic parameters of the project
# each time you want to stack data, you have to respect the same PER : PROJECTION - EXTENT - RESOLUTION
# it's the standard settings of the project.

## A. Upload a raster
ref.tile = raster (paste(act.climate.folder, "/", list.files(act.climate.folder)[2], sep = "")) # the first tile is the reference

# B. Project projection
p.proj = crs(ref.tile)

# C. Project extent
d.occ = read.csv(here::here ("data", "processed", "d.occ.csv"))
ext.matrix = matrix (nrow = 2, ncol = 2, 
                     byrow = T, 
                     data = c(min (d.occ$x) - abs(max (d.occ$x) - min (d.occ$x))*0.30,  # add a percentage to the real extent
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
tiles = list.files(path = paste (act.climate.folder)) # names of each tile

# Retain the `bio` variables
cur_tiles = grep("bio", tiles, value = TRUE)  

# load as a `stack`
cur.st = stack(paste(act.climate.folder, cur_tiles, sep = ""))

### B. Transform data
# Project projection

# Project extent
# crop to the spatial extent
# lim = as(ext.stud, Class = "Spatial")
# cur.st.m = raster::mask(cur.st, lim)
cur.st.c = raster::crop (cur.st, p.extent)

# Simplify the name
names(cur.st.c) = paste0("bio", 1:19)

# Project resolution 


#####
# 4. Past climate LGM
#####
### A. Import data
# List the files in the folder
tiles = list.files(path = paste (lgm.climate.folder)) # names of each tile

# Retain the `bio` variables
lgm_tiles = grep("bio", tiles, value = TRUE)  

# load as a `stack`
lgm.st = stack(paste(lgm.climate.folder, lgm_tiles, sep = ""))

### B. Transform data
# Project projection

# Project extent
# crop to the spatial extent
# lim = as(ext.stud, Class = "Spatial")
# cur.st.m = raster::mask(cur.st, lim)
lgm.st.c = raster::crop (lgm.st, p.extent)

# Project resolution 

#####
# 5. Past climate Mid Holocene
#####
### A. Import data
# List the files in the folder
tiles = list.files(path = paste (mholo.climate.folder)) # names of each tile

# Retain the `bio` variables
mholo_tiles = grep("bio", tiles, value = TRUE)  

# load as a `stack`
mholo.st = stack(paste(mholo.climate.folder, mholo_tiles, sep = ""))

### B. Transform data
# Project projection

# Project extent
# crop to the spatial extent
# lim = as(ext.stud, Class = "Spatial")
# cur.st.m = raster::mask(cur.st, lim)
mholo.st.c = raster::crop (mholo.st, p.extent)

# Project resolution 


#####
# 6. Elevation
#####

### A. Import data
tiles = list.files(path = paste (geo.folder)) # names of each tile
elevation = raster(paste(geo.folder, "wc2.1_30s_elev.tif", sep = ""))

### B. Transform data
# Project projection
crs(elevation)

# Project extent
elevation.c = raster::crop (elevation, p.extent)

# Project resolution 

#####
# 7. Assemble maps
#####

# Current
cur_env = stack (cur.st.c, 
                 elevation.c)

# Past
past_env = stack (past.st.c, 
                  elevation.c)


