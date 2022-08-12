#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 12.variable_selection.R
# Analysis of correlation among variables
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

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





