#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 06.import_data_miroecology.R
# Import ecology data (cbn and GP)
# Authors : Guillaume Papuga
# Last update : 26 July 2022
#######################################################

### Load data
# Load presence data

# Raster : define the path to the source folder
source = "/home/papuga/Documents/climate" # linux


##################################### Define the system ##############################################





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

