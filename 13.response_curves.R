#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 13.response_curves.R
# Draw the response curve of each variable
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

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

