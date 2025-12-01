#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 07.map.R
# Create a general map of the study area with species' occurrences
# Authors : Guillaume Papuga
# Last update : 26 July 2022
#######################################################


# -----------------------------
# 1. Zone d'étude : France métropole + péninsule ibérique + Portugal
# -----------------------------
countries <- ne_countries(scale = "medium", returnclass = "sf")

# ---- France métropole ----
france <- countries %>% filter(admin == "France")
metropole_rect <- st_polygon(list(rbind(c(-5,41), c(10,41), c(10,51), c(-5,51), c(-5,41))))
metropole_bbox <- st_sfc(metropole_rect, crs=st_crs(france)) %>% st_sf()
fr_metropole <- st_intersection(france, metropole_bbox)

# ---- Espagne (péninsule + Baléares) ----
espagne <- countries %>% filter(admin == "Spain")
peninsula_rect <- st_polygon(list(rbind(c(-10,35), c(5,35), c(5,44.5), c(-10,44.5), c(-10,35))))
peninsula_bbox <- st_sfc(peninsula_rect, crs=st_crs(espagne)) %>% st_sf()
espagne_peninsula <- st_intersection(espagne, peninsula_bbox)

# ---- Portugal (péninsule ibérique) ----
portugal <- countries %>% filter(admin == "Portugal")
portugal_peninsula <- st_intersection(portugal, peninsula_bbox)

# ---- Fusion study_area ----
study_area <- bind_rows(
  fr_metropole[, c("admin","geometry")],
  espagne_peninsula[, c("admin","geometry")],
  portugal_peninsula[, c("admin","geometry")])

# ---- Pays voisins pour contours ----
neighbors <- countries %>% filter(admin %in% c("Morocco","Algeria","Andorra"))

# -----------------------------
# 2. MNT réel avec elevatr (~90 m)
# -----------------------------
# Limites à garder pour coord_sf (zone principale)
xlim <- c(-10, 7.5)
ylim <- c(35, 45)

# Étendre bbox pour inclure Maroc et Algérie pour le raster
xlim_mnt <- c(-10, 7.5)
ylim_mnt <- c(28, 45)

bbox_mat_mnt <- matrix(
  c(xlim_mnt[1], ylim_mnt[1],
    xlim_mnt[2], ylim_mnt[1],
    xlim_mnt[2], ylim_mnt[2],
    xlim_mnt[1], ylim_mnt[2],
    xlim_mnt[1], ylim_mnt[1]),
  ncol = 2, byrow = TRUE)
bbox_poly_mnt <- st_polygon(list(bbox_mat_mnt)) %>% st_sfc(crs=4326) %>% st_sf()

# Télécharger MNT
mnt <- get_elev_raster(locations = bbox_poly_mnt, z = 5)
mnt_rast <- rast(mnt)

# Crop le raster sur study_area + voisins
crop_area <- st_union(study_area, neighbors)
mnt_crop <- terra::crop(mnt_rast, crop_area)


# Classer en 6 catégories
reclass_matrix <- cbind(
  c(-Inf, 0, 0, 100, 100, 300, 300, 500, 500, 1000, 1000, Inf),
  c(0, 0, 100, 300, 300, 500, 500, 1000, 1000, Inf, Inf, Inf),
  c(1, NA, 2, 3, 4, 5, 6, 6, 6, 6, 6, 6)  # NA = mer
  )

alt_cat <- classify(mnt_crop, reclass_matrix)
alt_df <- as.data.frame(alt_cat, xy=TRUE)
names(alt_df)[3] <- "elev_class"

# # -----------------------------
# # 3. Villes principales
# # -----------------------------
# cities <- data.frame(
#   name = c("Marseille", "Montpellier", "Barcelona", "Zaragoza", 
#            "Madrid", "Valencia", "Seville"),
#   lon = c(5.3698, 3.8767, 2.1734, -0.8787, -3.7038, -0.3763, -5.9845),
#   lat = c(43.2965, 43.6117, 41.3851, 41.6488, 40.4168, 39.4699, 37.3891)
# )
# cities$label <- LETTERS[1:nrow(cities)]
# cities_sf <- st_as_sf(cities, coords = c("lon","lat"), crs=4326)

# -----------------------------
# 4. Occurrences de l'espèce
# -----------------------------
d.occ <- read.csv(here::here("data", "processed", "d.occ.csv"), sep=",", header=TRUE, dec=".")
occ_sf <- st_as_sf(d.occ, coords = c("x","y"), crs=4326)

# -----------------------------
# 4. Buffer d'occurrence
# -----------------------------

# Transformer en projection métrique (UTM ou Mollweide)
occ_sf_m <- st_transform(occ_sf, crs = 3857)  # EPSG:3857 = Mercator en mètres

# Créer le buffer de 10 km = 10 000 m
occ_buffer <- st_buffer(occ_sf_m, dist = 50000)

# Fusionner tous les polygones qui se touchent
buffer_union <- st_union(occ_buffer)

# Reprojeter en WGS84 pour visualisation ou export
buffer_union_wgs <- st_transform(buffer_union, crs = 4326)

# Recouper la terre
buffer_on_land <- st_intersection(buffer_union_wgs, countries)

# Plot
plot(buffer_on_land)

# -----------------------------
# 7. Carte finale
# -----------------------------

alt_df <- as.data.frame(mnt_crop, xy = TRUE)
colnames(alt_df) <- c("x", "y", "elev") 
alt_df$elev[alt_df$elev < 0] <- NA


map_methods = ggplot() +
  # Relief continu
  geom_raster(data = alt_df, aes(x = x, y = y, fill = elev)) +
  scale_fill_gradient(
    low = "grey100",   # bas
    high = "grey1",  # haut
    na.value = "transparent",
    name = "Elevation (m)"
  ) +
  
  # Frontières pays
  geom_sf(data = study_area, fill = NA, color = "black", size = 0.3) +
  geom_sf(data = neighbors, fill = NA, color = "black", size = 0.3) +
  
  # Buffer
  geom_sf(data = buffer_on_land, fill = "#1b9e77", alpha = 0.4) +
  
  # Points d'occurrence
  geom_sf(data = occ_sf, color = "#d95f02", size = 0.5, alpha = 0.9) +
  
  # Cadre noir
  geom_sf(data = bbox_poly, fill = NA, color = "black", size = 0.8) +
  
  # Coordonnées
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  )

plot(map_methods)


## Save the graph
ggsave(
  filename = here::here("outputs", "figures", "map_methods.jpg"),   # chemin + nom du fichier
  plot = map_methods,                    # objet ggplot
  width = 12, height = 8,      # dimensions (en pouces)
  dpi = 300                    # résolution
)

