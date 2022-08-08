#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 05.import_data_raster.R
# Import and assemble rasters
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################


#####
# 1. Uploading variables
#####

tile = list.files(path = paste(source, "/current/wc2.1_10m_bio", sep = "")) # names of each tile
cur.st = stack(paste(source, "/current/wc2.1_10m_bio/",tile, sep = ""))



# a. Potential Evapo Transpiration PET
pet.an = raster ("/media/papuga/TOSHIBA EXT/02.spatial.data/03.environement/climat/pyrenees/PET/PET_Annual/PET_Annual_Pyrenees.tif")




##################################### Define the system ##############################################






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


