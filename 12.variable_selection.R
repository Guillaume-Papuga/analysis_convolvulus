#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 12.variable_selection.R
# Analysis of correlation among variables
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

### Load data 
df = read.csv(here::here ("data", "processed", "sb_data_cast.csv"), 
              head = T, sep = ",", dec = ".")


######################### A. Global analysis of the complete dataset ######################################
### create the table that summarize the raw analysis
dist = ecodist::distance(df[,-(1:3)], "bray-curtis") # create a distance matrix



# Upload data

# Multivariate analysis

# Multiple correlation

# Response curve