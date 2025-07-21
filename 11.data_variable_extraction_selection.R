#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 11.data_variable_extraction.R
# Combine datasets and extract environmental data
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

#####
# 0. Load data
#####
u.occ = read.csv(here::here("data", "processed", "u.occ.csv"), # upload data
                 sep = ",", header = T, dec = ".", row.names = 1)

u.abs = read.csv(here::here("data", "processed", "u.abs.csv"), # upload data
                 sep = ",", header = T, dec = ".", row.names = 1)

#####
# 1. Build the complete table
#####
# Combine the two datasets
dt = bind_rows(u.occ %>% dplyr::select(presence, x, y), 
               u.abs %>% dplyr::select(presence, x, y)) 

# Extract data
env.tab = as.data.frame(raster::extract (cur_env, 
                                         dt %>% dplyr::select(x, y))) %>%
  mutate(presence = dt$presence) # add the presence column

# Rename columns
names(env.tab) = names(env.tab) %>%
  gsub("CHELSA.", "", .) %>%
  gsub("_1981.2010_V.2.1", "", .) %>%
  gsub("wc2.1_30s_elev", "elev", .)

# Save the dataset
write.csv(env.tab, 
          here::here ("data", "processed", "env.tab.csv"))

#####
# 0. Load data
#####
env.tab = read.csv(here::here("data", "processed", "env.tab.csv"), # upload data
                   sep = ",", header = T, dec = ".", row.names = 1)

#####
# 1. Multivariate analysis
#####
# Build the presence table
env = env.tab %>% 
  filter (presence == 1) %>%
  dplyr::select(-presence) %>%
  na.omit()

# Run a PCA on climate data
pca.env = dudi.pca(env, scannf = F, nf = 2)

# Plot the correlation circle
s.corcircle (pca.env$co)

# Save the image 
png(here::here("outputs", "figures", "corcircle.current.clim.png")) # open an empty png
s.corcircle (pca.env$co)
dev.off() # end the process

#####
# 2. Multiple correlation
#####





#####
# 0. Load data
#####
# Raw data
env.tab = read.csv(here::here("data", "processed", "env.tab.csv"), # upload data
                   sep = ",", header = T, dec = ".", row.names = 1)

# Melt the dataframe
env.tab.m = env.tab %>%
  melt(id = "presence")

# First autocorrelation filter (delete redondant variables)

#####
# 1. Response curve
#####

test = env.tab.m %>% filter (variable == "bio18")

ggplot(data = test, aes (x = value, y = presence)) +
  geom_point(alpha = 0.01) + 
  geom_smooth() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), col = "red", se = F) 

