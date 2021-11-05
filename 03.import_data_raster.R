#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 03.import_data_raster.R
# Import and assemble rasters
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

### Load data
# Load presence data

# Raster : define the path to the source folder
source = "/home/papuga/Documents/climate" # linux


##################################### Define the system ##############################################


# Bioclim variables for current climate
## 1. Upload from computer
tile = list.files(path = paste(source, "/current/wc2.1_10m_bio", sep = "")) # names of each tile
cur.st = stack(paste(source, "/current/wc2.1_10m_bio/",tile, sep = ""))

## 2. Plot
plot(cur.st$wc2.1_10m_bio_1)


## b. Define the basic setting of raster layers
## Set the basic parameters of the project
# each time you want to stack data, you have to respect the same PER : PROJECTION - EXTENT - RESOLUTION
# it's called the standard settings of the project.

# Same projection
p.proj = crs(cur.st)

# Same extent
p.extent = upload the extent from the previous script

# Same resolution 
p.res = res(cur.st)

##################################### I. Model 1 ##############################################

# Upload and work on layers
# Ecological variables
## LGM
tile = list.files(path = paste(source, "/lgm/cclgmbi_10m", sep = "")) # names of each tile
lgm_cc.st = stack(paste(source, "/lgm/cclgmbi_10m/",tile, sep = "")) # stack the tiles


# crop to spatial extent
lim = as(ext.stud, Class = "Spatial")
cur.st.m = raster::mask(cur.st, lim)
cur.st.c = raster::crop (cur.st.m, lim)

##################################### I. Model 1 ##############################################

# Assemble layers


