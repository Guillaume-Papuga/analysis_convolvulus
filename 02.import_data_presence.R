#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 02.import_data_presence.R
# Import, sort and assemble presence data
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################


## 1. Dataset structure
```{r}
# The matrix is named `d.occ` and must contain 6 columns
# code.pop : population code (dataset specific)
# sp.name : species names (Convolvulus lanuginosus)
# presence : 1-0 for presence-(pseudo)absence
# date : the date (yyyymmdd)
# x : coordinate on the x-axis (longitude in decimal degree)
# y : coordinate on the y-axis (latitude in decimal degree)
# precision : precision of the location (expressed in meter)
# source : explicit name of the source of the imported dataset

d.occ.raw = as.data.frame(matrix (ncol = 8,nrow = 0))
colnames (d.occ.raw) = c("code.pop", "sp.name", "presence", "date", "x", "y", "precision", "source")

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


