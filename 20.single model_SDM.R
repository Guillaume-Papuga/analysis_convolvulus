#######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 20.glm_distribution_model.R
# GLM
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################

### Load data 
df = read.csv(here::here ("data", "processed", "sb_data_cast.csv"), 
              head = T, sep = ",", dec = ".")

#######################################################################################
################################## # GLM  #############################################
#######################################################################################

### Single GLM model
# Select variables

# Evaluate the model

# Split - evaluate process to account for the variance due to pseudo-absence

# Save the model

# Represent the model

# Project
# Present
# Futur

### Join and average


##############"" Test 1 ####################"
### Load data 
df = read.csv(
  here::here("data", "processed", "sb_data_cast.csv"),
  head = TRUE, sep = ",", dec = "."
)

#######################################################################################
################################### GLM  ##############################################
#######################################################################################

library(dplyr)
library(pROC)
library(terra)
library(ggplot2)

### Single GLM model
#-------------------------

# Select variables
# Supposons que df contient :
# presabs = 0/1
# x, y   = coordonnées
# bio1, bio12, etc. = variables environnementales

env_vars <- c("bio1", "bio12", "bio5", "bio6")  # exemple
df_mod <- df[, c("presabs", env_vars)]

# Convert presence/absence to factor
df_mod$presabs <- as.factor(df_mod$presabs)


### Evaluate the model
#-------------------------

# Train/test split
set.seed(123)
train_id <- sample(1:nrow(df_mod), 0.7 * nrow(df_mod))
train <- df_mod[train_id, ]
test  <- df_mod[-train_id, ]

# Fit GLM
glm_mod <- glm(
  presabs ~ ., 
  data = train,
  family = binomial
)

summary(glm_mod)

# Predict on test
test$pred <- predict(glm_mod, test, type = "response")

# AUC
auc_value <- pROC::roc(test$presabs, test$pred)$auc
auc_value

# Confusion matrix at threshold = 0.5
test$pred_class <- ifelse(test$pred > 0.5, 1, 0)
TSS <- (sum(test$pred_class == 1 & test$presabs == 1) / sum(test$presabs == 1)) +
  (sum(test$pred_class == 0 & test$presabs == 0) / sum(test$presabs == 0)) - 1
TSS


### Split - evaluate process to account for variance due to pseudo-absence
#--------------------------------------------------------------------------

# Suppose que tu as besoin de générer plusieurs jeux de pseudo-absences.
# Ici, on répète 20 fois le modèle en tirant des pseudo-absences différentes.

nrep <- 20
results <- list()
auc_values <- c()

for (i in 1:nrep) {
  # Générer pseudo-absences aléatoires
  abs_df <- df %>% filter(presabs == 0) %>% sample_n(sum(df$presabs == 1))
  
  pa_df <- df %>% filter(presabs == 1)
  mix_df <- rbind(pa_df, abs_df)
  mix_df$presabs <- as.factor(mix_df$presabs)
  
  # Train/test
  train_id <- sample(1:nrow(mix_df), 0.7 * nrow(mix_df))
  train <- mix_df[train_id, ]
  test  <- mix_df[-train_id, ]
  
  # Fit GLM
  mod <- glm(presabs ~ ., data = train[, c("presabs", env_vars)], family = binomial)
  
  # Predict
  test$pred <- predict(mod, test, type = "response")
  auc_values[i] <- pROC::roc(test$presabs, test$pred)$auc
  
  results[[i]] <- mod
}

mean_auc <- mean(auc_values)
mean_auc
sd_auc <- sd(auc_values)
sd_auc


### Save the model
saveRDS(results, here::here("models", "glm_sdm_models.rds"))


### Represent the model (response curves)
#--------------------------------------------------------

# Example: response curve for bio1
bio1_seq <- seq(min(df$bio1), max(df$bio1), length = 200)
pred_df <- data.frame(
  bio1 = bio1_seq,
  bio12 = mean(df$bio12),
  bio5 = mean(df$bio5),
  bio6 = mean(df$bio6)
)

pred_df$pred <- predict(glm_mod, pred_df, type = "response")

ggplot(pred_df, aes(x = bio1, y = pred)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "bio1", y = "Probabilité de présence")


### Project
#-----------------------------------

# Load present climate
clim_present <- rast("data/climate/present/*.tif")  # stack of env variables
names(clim_present) <- env_vars

# Projection
pred_present <- predict(clim_present, glm_mod, type = "response")
writeRaster(pred_present, "outputs/SDM_present.tif", overwrite = TRUE)


# Load future climate
clim_future <- rast("data/climate/future/*.tif")
names(clim_future) <- env_vars

pred_future <- predict(clim_future, glm_mod, type = "response")
writeRaster(pred_future, "outputs/SDM_future.tif", overwrite = TRUE)


### Join and average (across pseudo-absence runs)
#----------------------------------------------------------

# Charger les modèles sauvegardés
mods <- readRDS(here::here("models", "glm_sdm_models.rds"))

# Projeter chaque modèle
preds <- list()

for (i in 1:length(mods)) {
  preds[[i]] <- predict(clim_present, mods[[i]], type = "response")
}

# Moyenne des projections
mean_pred <- mean(rast(preds))
writeRaster(mean_pred, "outputs/SDM_present_mean.tif", overwrite = TRUE)


######## Avec ENWeval
### Load data 
library(here)
library(dplyr)
library(terra)
library(ENMeval)
library(ggplot2)

df = read.csv(
  here::here("data", "processed", "sb_data_cast.csv"),
  head = TRUE, sep = ",", dec = "."
)

# Variables environnementales
env_vars <- c("bio1", "bio12", "bio5", "bio6")
df_mod <- df[, c("presabs", env_vars)]
df_mod$presabs <- as.factor(df_mod$presabs)

# Préparer matrices pour ENMeval
# ENMeval attend présence/pseudo-absence en 1/0 et raster de covariables
pres_points <- df_mod %>% filter(presabs == 1)
abs_points  <- df_mod %>% filter(presabs == 0)

# Créer un raster stack avec les variables
r <- rast(df[, env_vars])  # Si tu as des raster layers correspondants
names(r) <- env_vars

# Si pas de raster disponible, ENMeval peut fonctionner sur points + data.frame

### ENMevaluate GLM
set.seed(123)

eval_glm <- ENMevaluate(
  occ = pres_points[, c("x", "y")],       # coords présence
  env = r,                                 # raster stack environnement
  bg.coords = abs_points[, c("x", "y")],  # pseudo-absences
  method = 'block',                        # split spatial pour évaluation
  algorithm = 'glm',                       # modèle GLM
  fc = c("L","Q"),                         # linear / quadratic
  RMvalues = 1,                            # regularization multiplier
  parallel = TRUE
)

# Meilleur modèle selon AICc
best_model <- eval_glm@models[[which.min(eval_glm@results$AICc)]]

# Inspecter résultats
eval_glm@results

### Représenter le modèle (response curve)
pred_df <- data.frame(
  bio1 = seq(min(df$bio1), max(df$bio1), length = 200),
  bio12 = mean(df$bio12),
  bio5 = mean(df$bio5),
  bio6 = mean(df$bio6)
)

pred_df$pred <- predict(best_model, pred_df, type = "response")

ggplot(pred_df, aes(x = bio1, y = pred)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "bio1", y = "Probabilité de présence")

### Projection sur climat actuel
pred_present <- predict(r, best_model, type = "response")
writeRaster(pred_present, "outputs/SDM_present_ENMeval.tif", overwrite = TRUE)

### Projection sur climat futur
# chargement raster futur
clim_future <- rast("data/climate/future/*.tif")
names(clim_future) <- env_vars
pred_future <- predict(clim_future, best_model, type = "response")
writeRaster(pred_future, "outputs/SDM_future_ENMeval.tif", overwrite = TRUE)

