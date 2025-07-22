#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 06.import_data_genetic.R
# Import Genetic data (cbn and GP)
# Authors : Guillaume Papuga
# Last update : 21 July 2025
#######################################################

#####
# 1. Genetic Data
#####

# Load data
genetic.data = read.csv(here::here("data", "raw", "genetic", "SSR Data_429 Liseron_gp_25.csv"), # upload data
                       sep = ";", header = T, dec = ",")
genetic.data
str(genetic.data)

### save the dataset
write.csv(genetic.data, 
          here::here ("data", "processed", "genetic.data.csv"))

#####
# 2. 
#####


