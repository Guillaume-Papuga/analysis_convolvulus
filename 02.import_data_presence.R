#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 02.import_data_presence.R
# Import, sort and assemble presence data
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

#####
# 0. Define dataset characteristics
#####
# The matrix is named `d.occ` and must contain 6 columns
  # code.pop : population code (dataset specific)
  # sp.name : species names (e.g. Convolvulus lanuginosus)
  # presence : 1-0 for presence-(pseudo)absence
  # date : the date ()
  # x : coordinate on the x-axis (longitude in decimal degree)
  # y : coordinate on the y-axis (latitude in decimal degree)
  # precision : precision of the location (expressed in meter)
  # source : explicit name of the source of the imported dataset

d.occ.raw = as.data.frame(matrix (ncol = 8,nrow = 0))
colnames (d.occ.raw) = c("code.pop", "sp.name", "presence", "date", "x", "y", "precision", "source")

#####
# 1. CBN Med
#####
# Load data
d.occ_cbn = read.csv(here::here("data", "raw", "presence", "occ.cbn.convol_lan.260722.csv"), # upload data
                     sep = "", header = T)

# Show the columns to select
# str(d.occ_cbn) 

# Filter data
d.occ_cbn = d.occ_cbn %>% 
  mutate (sp.name = rep("Convolvulus lanuginosus", nrow(d.occ_cbn)), # add the name of the species
          presence = rep(1, nrow(d.occ_cbn)),
          source = rep("cbn", nrow(d.occ_cbn))) %>% # add the `presence` column
  mutate(date = as.Date(date_releve_deb, format =  "%Y-%m-%d")) %>% # transform the date into a `date` format
  
  dplyr::select ("id_observation", "sp.name", "presence", "date", "lon_wgs84", "lat_wgs84", "id_precision", "source") %>% # select the columns
  dplyr::rename ("code.pop" = "id_observation", # rename the variables
          "x" = "lon_wgs84", 
          "y" = "lat_wgs84", 
          "precision" = "id_precision") %>% # name each variable correctly
  
  tidyr::drop_na () %>% # delete rows with NA
  filter (precision == "P", 
          date > "1990-01-01") # %>%
  # refaire la colonne precision


  #arrange (year)

#####
# 2. GBIF
#####



#####
# 3. iNat
#####


#####
# 4. Anthos
#####



#####
# 5. Assemble
#####

##################### I. GBIF #################################
# Upload the dataset

# Sort and filter

# load raw data
data.cbn = read.csv(here::here("data","raw", "export_360_28012020_cbnmed.csv"), sep = "\t", dec = ".")

# select correct columns
d.occ.cbn = data.cbn %>%
  mutate (presence = 1) %>%
  mutate (source = "cbnmed") %>%
  dplyr::select(c(id_observation, nom_reconnu, presence, date_releve_fin, lon_wgs84, lat_wgs84, id_precision, source)) %>%
  mutate(id_precision = case_when(id_precision  == 'P' ~ 10,
                                  id_precision  == 'T' ~ 500,
                                  id_precision  == 'C' ~ 10000,
                                  id_precision  == 'N' ~ 10000)) %>%
  rename (code.pop = id_observation, 
          sp.name = nom_reconnu, 
          date = date_releve_fin, 
          x = lon_wgs84, 
          y = lat_wgs84, 
          precision = id_precision)

# process & correct
d.occ.cbn = unique (d.occ.cbn) # delete duplicates

# change the format of the date

## filter for the date and the precision
d.occ.cbn = d.occ.cbn %>%
  drop_na() %>% # remove NA
  filter (precision < 1001)# precision
# date


##################### Assemble #################################

### Assemble sort and filter the complete dataset

### Project and define the study area

# Plot

# Draw and save the extent

#### Restrain the dataset to the extent of the project


### Create a summary table of the dataset




#####
# 6. Assembling the dataset
#####

### Presentation
head (cbnmed.melt.pa)
head (ub.melt.pa)
head (por.melt.pa)
head (cat.melt.pa)
head (arb.melt.pa)

### Joining the tables
sb_data = cbnmed.melt.pa %>%
  bind_rows(ub.melt.pa, por.melt.pa, cat.melt.pa, arb.melt.pa)

### Cleaning the dataset
sb_data = sb_data %>%
  mutate (combe = str_to_lower(combe), 
          quad = str_to_lower(quad),
          variable = str_to_lower(variable)) # simplify the writing to lower case

sb_data = sb_data %>%
  filter(variable != "delete") # delete all the taxa we excluded from the analysis with "delete"

missing_sp = sb_data %>%
  group_by(variable) %>%
  dplyr::summarize (n_obs = sum(value.pa)) %>%
  filter(n_obs == 0) # give the list of species for which there is no observation
missing_sp = as.vector(missing_sp$variable)
`%notin%` <- Negate(`%in%`) # creating a not in operator
sb_data = sb_data %>%
  filter(variable %notin% missing_sp) # delete all the taxa we showed no data

### Compute the frequency for each species (= variable) on each site (= combe) per plot (= placette) for each year
sb_data_freq = sb_data %>%
  group_by(combe, placette, year, variable) %>%
  dplyr::summarize (n_obs = sum(value.pa)/12)

### Cast the dataset into a large format with species as variables
sb_data_cast = dcast(sb_data_freq, ... ~ variable)# %>% # structure the dataset
sb_data_cast[is.na(sb_data_cast)] = 0 # replace NA with 0

### Delete empty rows = quadrat that were not followed due to snow, etc.
sb_data_cast = sb_data_cast %>%
  mutate (obs_quad = rowSums(sb_data_cast[, -(1:4)])) %>%
  filter(obs_quad != 0) %>%
  dplyr::select (-obs_quad)

as.data.frame(table(sb_data_cast[,1:3])) %>%
  arrange(combe, placette)

### save the dataset
write_csv( sb_data_cast, 
           here::here ("data", "processed", "sb_data_cast.csv"))


