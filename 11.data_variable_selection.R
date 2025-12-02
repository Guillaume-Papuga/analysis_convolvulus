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
  mutate(presence = dt$presence)  %>% # add the presence column
  na.omit()

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
  dplyr::select(-presence)

# Run a PCA on climate data
pca.env = dudi.pca(env, scannf = F, nf = 2)

# Plot the correlation circle
s.corcircle (pca.env$co)

# Save the image 
png(here::here("outputs", "figures", "corcircle.current.clim.png")) # open an empty png
s.corcircle (pca.env$co)
dev.off() # end the process

# Plot the presence/absence

# Préparation des données pour ggplot
df <- pca.env$li %>%
  mutate(presabs = factor(env.tab$presence))

# Calcul des barycentres
bary <- df %>%
  group_by(presabs) %>%
  summarise(across(c(Axis1, Axis2), mean))

# Plot GGplot
plot_p_a = ggplot(df, aes(x = Axis1, y = Axis2, color = presabs)) +
  # 👉 Axes X et Y
  geom_hline(yintercept = 0, linewidth = 0.8, color = "grey60") +
  geom_vline(xintercept = 0, linewidth = 0.8, color = "grey60") +
  geom_point(size = 0.4, alpha = 0.3) +
  stat_ellipse(level = 0.68, linewidth = 1) +
  geom_point(data = bary, aes(x = Axis1, y = Axis2), 
             size = 4, color = "black") +
  geom_label(data = bary, aes(label = presabs),
             fill = "white", size = 6, fontface = "bold",
             label.size = 0.5, label.r = unit(0.2, "lines")) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "tomato")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  xlab("Axe 1") +
  ylab("Axe 2") +
  ggtitle("ACP – Présence / Absence")

plot_p_a
#####

# Save the image 
png(here::here("outputs", "figures", "plot_pa_pca.png")) # open an empty png
plot_p_a
dev.off() # end the process

#####
# 2. Multiple correlation
#####

library(ggcorrplot)

M <- cor(env)
ggcorrplot(M, lab = TRUE, type = "upper")


###############"
library(caret)

corr_matrix <- cor(env)
highcorr <- findCorrelation(corr_matrix, cutoff = 0.7)
highcorr




#####
# 1. Response curve
#####

library(patchwork)

# Couleurs identiques à ton ACP
cols <- c("0" = "steelblue", "1" = "tomato")

# Réorganisation en long format
df_long <- env %>%
  mutate(presabs = factor(env.tab$presence)) %>%
  pivot_longer(cols = -presabs, names_to = "variable", values_to = "value")

# Fonction pour produire un graphique par variable
plot_var <- function(varname) {
  df_long %>%
    filter(variable == varname) %>%
    ggplot(aes(x = value, fill = presabs)) +
    geom_density(alpha = 0.4, adjust = 1, linewidth = 1, color = NA) +
    scale_fill_manual(values = cols) +
    labs(title = varname, x = "", y = "Densité") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

# Générer la liste des graphes
plots <- lapply(unique(df_long$variable), plot_var)
plots


