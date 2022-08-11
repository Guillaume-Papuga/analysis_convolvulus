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

# Set the correct format
d.occ.raw = d.occ.raw %>%
  mutate(code.pop = as.integer(code.pop), 
         sp.name = as.character(sp.name), 
         presence = as.numeric(presence), 
         date = as.Date(date),
         x = as.numeric(x), 
         y = as.numeric(y), 
         precision = as.numeric(precision), 
         source = as.character(source))

#####
# 1. CBN Med
#####
# Load data
raw.occ_cbn = read.csv(here::here("data", "raw", "presence", "occ.cbn.convol_lan.260722.csv"), # upload data
                       sep = "\t", header = T, dec = ".")

# Identify the relevant columns
# str(raw.occ_cbn) 

# Format the dataset
d.occ_cbn = raw.occ_cbn %>% 
  # Add columns
  mutate (sp.name = "Convolvulus lanuginosus", # add the name of the species
          presence = 1,
          source = "cbnmed") %>% # add the `presence` column
  # Transform the date into a `date` format
  mutate(date = as.Date(date_releve_deb, format =  "%Y-%m-%d")) %>% 
  # Modify the precision column
  mutate(id_precision = case_when(id_precision  == 'P' ~ 10,
                                  id_precision  == 'T' ~ 500,
                                  id_precision  == 'C' ~ 10000,
                                  id_precision  == 'N' ~ 10000)) %>%
  # Select variables
  dplyr::select ("id_observation",  # select the columns
                 "sp.name", 
                 "presence", 
                 "date", 
                 "lon_wgs84", 
                 "lat_wgs84", 
                 "id_precision", 
                 "source") %>%
  dplyr::rename ("code.pop" = "id_observation", # rename the variables
                 "x" = "lon_wgs84", 
                 "y" = "lat_wgs84", 
                 "precision" = "id_precision") %>% 
  # Filter
  tidyr::drop_na () %>% # delete rows with NA
  filter (precision < 1000 , 
          date > "1990-01-01") %>%
  # Arrange (year)
  arrange (date)

#####
# 2. GBIF
#####
# Load data
raw.occ_gbif = read.csv(here::here("data", "raw", "presence", "occ.gbif.convol_lan.260722.csv"), # upload data
                        sep = "\t", header = T, dec = ".")

# Identify the relevant columns
# str(raw.occ_gbif) 

# Format the dataset
d.occ_gbif = raw.occ_gbif %>% 
  # Add columns
  mutate (sp.name = "Convolvulus lanuginosus", # add the name of the species
          presence = 1,
          source = "gbif") %>% # add the `presence` column
  # Transform the date into a `date` format
  mutate(date = as.Date(eventDate, format =  "%Y-%m-%d")) %>% 
  # Select variables
  dplyr::select ("gbifID",  # select the columns
                 "sp.name", 
                 "presence", 
                 "date", 
                 "decimalLongitude", 
                 "decimalLatitude", 
                 "coordinateUncertaintyInMeters", 
                 "source") %>%
  dplyr::rename ("code.pop" = "gbifID", # rename the variables
                 "x" = "decimalLongitude", 
                 "y" = "decimalLatitude", 
                 "precision" = "coordinateUncertaintyInMeters") %>% 
  # Filter
  tidyr::drop_na () %>% # delete rows with NA
  filter (precision < 1000 , 
          date > "1990-01-01") %>%
  # Arrange (year)
  arrange (date)

#####
# 3. iNat
#####
# Load data
raw.occ_inat = read.csv(here::here("data", "raw", "presence", "occ.inat.convol_lan.010822.csv"), # upload data
                       sep = ",", header = T, dec = ".")

# Identify the relevant columns
# str(raw.occ_inat) 

# Format the dataset
d.occ_inat = raw.occ_inat %>% 
  # Add columns
  mutate (sp.name = "Convolvulus lanuginosus", # add the name of the species
          presence = 1,
          source = "inat") %>% # add the `presence` column
  # Transform the date into a `date` format
  mutate(date = as.Date(observed_on, format =  "%Y-%m-%d")) %>% 
  # Select variables
  dplyr::select ("id",  # select the columns
                 "sp.name", 
                 "presence", 
                 "date", 
                 "longitude", 
                 "latitude", 
                 "positional_accuracy", 
                 "source") %>%
  dplyr::rename ("code.pop" = "id", # rename the variables
                 "x" = "longitude", 
                 "y" = "latitude", 
                 "precision" = "positional_accuracy") %>% 
  # Filter
  tidyr::drop_na () %>% # delete rows with NA
  filter (precision < 1000 , 
          date > "1990-01-01") %>%
  # Arrange (year)
  arrange (date)

#####
# 4. Assembling the dataset
#####

# Assemble-filter the complete dataset
d.occ = d.occ.raw %>%
  bind_rows(d.occ_cbn, d.occ_gbif, d.occ_inat) %>% 
  distinct(x, y, .keep_all= TRUE) # eliminate potential duplicate from different databases

# Identify and delete outliers in a plot
X11(width=10, height=10) # Rstudio graphic window doesnt support `identify`
plot(d.occ$x, d.occ$y, # set the plot
     ylim = c(min (d.occ$y)*0.95, max (d.occ$y)*1.05),
     xlim = c(min (d.occ$x)*0.95, max (d.occ$x)*1.05),
     pch = 19, cex = 0.3) 

outliers = identify(d.occ$x, d.occ$y, # vectors of identified outliers
                    tolerance = 0.8, 
                    labels = d.occ$code.pop, atpen = T)

d.occ = d.occ %>% 
  slice(-outliers) # delete outliers

# Save the final version
write.csv(d.occ, 
          here::here ("data", "processed", "d.occ.csv"))

#####
# 5. Summary table of the dataset
#####
# Create the table
synth.tab = data.frame(t(table(d.occ$source))) %>%
  
  dplyr::select(Var2, Freq) %>%
  dplyr::rename ("Source" = "Var2", # rename the variables
                 "N_unfiltered" = "Freq") 

# Create the folder
# dir.create (path = here::here("outputs", "tables"))

# Write the table
write.csv(synth.tab, 
          here::here ("outputs", "tables", "synth.tab.csv"))

