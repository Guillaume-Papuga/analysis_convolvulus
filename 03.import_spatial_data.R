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
climate.folder = "/home/papuga/Bureau/chelsa/actual/" # climate folder for the project
act.climate.folder= "/home/papuga/Bureau/chelsa/actual/" # climate folder for the project
past.climate.folder = "/home/papuga/Bureau/chelsa/actual/" # climate folder for the project
geo.folder = "/home/papuga/Bureau/geography/" 

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

# Project resolution 

#####
# 4. Past climate
#####
### A. Import data
# List the files in the folder
tiles = list.files(path = paste (past.climate.folder)) # names of each tile

# Retain the `bio` variables
past_tiles = grep("bio", tiles, value = TRUE)  

# load as a `stack`
past.st = stack(paste(past.climate.folder, past_tiles, sep = ""))

### B. Transform data
# Project projection

# Project extent
# crop to the spatial extent
# lim = as(ext.stud, Class = "Spatial")
# cur.st.m = raster::mask(cur.st, lim)
past.st.c = raster::crop (past.st, p.extent)

# Project resolution 



#####
# 5. Elevation
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
# 6. Assemble maps
#####

# Current
cur_env = stack (cur.st.c, 
                 elevation.c)

# Past
past_env = stack (past.st.c, 
                  elevation.c)


