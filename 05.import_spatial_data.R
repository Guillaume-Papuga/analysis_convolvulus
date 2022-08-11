#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 05.import_spatial_data.R
# Import and assemble rasters
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

#####
# 1. Current climate
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
# 2. Past climate
#####

#####
# 3. Elevation
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
# 4. Assemble maps
#####

# Current
cur_env = stack (cur.st.c, 
                 elevation.c)

# Past

