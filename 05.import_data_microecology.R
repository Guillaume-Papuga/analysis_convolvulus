#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 06.import_data_microecology.R
# Import ecology data (cbn and GP)
# Authors : Guillaume Papuga
# Last update : 26 July 2022
#######################################################

#####
# 1. CBN phytosociological relevés
#####

# Load data
micro.data = read.csv(here::here("data", "raw", "ecology", "releve_conv.lan_cbn.csv"), # upload data
                       sep = "\t", header = T, dec = ".")

micro.meta = read.csv(here::here("data", "raw", "ecology", "releve_conv.lan_cbn_metadata.csv"), # upload data
                      sep = "\t", header = T, dec = ".")

# # Identify the relevant columns
# # str(raw.occ_cbn) 
# 
# # Format the dataset
# d.occ_cbn = raw.occ_cbn %>% 
#   # Add columns
#   mutate (sp.name = "Convolvulus lanuginosus", # add the name of the species
#           presence = 1,
#           source = "cbnmed") %>% # add the `presence` column
#   # Transform the date into a `date` format
#   mutate(date = as.Date(date_releve_deb, format =  "%Y-%m-%d")) %>% 
#   # Modify the precision column
#   mutate(id_precision = case_when(id_precision  == 'P' ~ 10,
#                                   id_precision  == 'T' ~ 500,
#                                   id_precision  == 'C' ~ 10000,
#                                   id_precision  == 'N' ~ 10000)) %>%
#   # Select variables
#   dplyr::select ("id_observation",  # select the columns
#                  "sp.name", 
#                  "presence", 
#                  "date", 
#                  "lon_wgs84", 
#                  "lat_wgs84", 
#                  "id_precision", 
#                  "source") %>%
#   dplyr::rename ("code.pop" = "id_observation", # rename the variables
#                  "x" = "lon_wgs84", 
#                  "y" = "lat_wgs84", 
#                  "precision" = "id_precision") %>% 
#   # Filter
#   tidyr::drop_na () %>% # delete rows with NA
#   filter (precision < 1000 , 
#           date > "1990-01-01") %>%
#   # Arrange (year)
#   arrange (date)
# 

### save the dataset
write_csv( data, 
           here::here ("data", "processed", "micro.data.cbn.csv"))


#####
# 2. GP microecological data
#####

# Load data
gp.microeco = read.csv(here::here("data", "raw", "ecology", "releve_conv.lan_cbn.csv"), # upload data
                       sep = "\t", header = T, dec = ".")



