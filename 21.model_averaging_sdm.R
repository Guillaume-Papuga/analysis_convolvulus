######################################################
# Project : Genetic analysis of Convolvulus lanuginosus
# Script : 23.model_averaging_sdm.R
# Different models from r-spatial
# Authors : Guillaume Papuga
# Last update : 5 november 2021
#######################################################


A VALIDER!!

### Load data
library(Biomod2)
library(dplyr)
library(terra)
library(here)

df <- read.csv(
  here::here("data", "processed", "sb_data_cast.csv"),
  head = TRUE, sep = ",", dec = "."
)

# Variables environnementales
env_vars <- c("bio1", "bio12", "bio5", "bio6")
df_mod <- df[, c("presabs", env_vars, "x", "y")]

# Préparer data pour Biomod
resp_var <- df_mod$presabs
expl_vars <- df_mod[, env_vars]

# Coordonnées
xy <- df_mod[, c("x", "y")]

### Create BIOMOD_Data object
biomod_data <- BIOMOD_FormatingData(
  resp.var = resp_var,
  expl.var = expl_vars,
  resp.xy  = xy,
  resp.name = "Species_X",
  PA.nb.rep = 1,        # nombre de jeux de pseudo-absences
  PA.nb.absences = sum(resp_var == 1) * 2,  # double des présences
  PA.strategy = 'random'
)

### Modeling options (GLM simple)
mod_options <- BIOMOD_ModelingOptions(
  GLM = list(type = 'quadratic', interaction.level = 0, test = 'AIC')
)

### Run models
set.seed(123)
biomod_model <- BIOMOD_Modeling(
  data = biomod_data,
  models = c("GLM"),
  models.options = mod_options,
  NbRunEval = 5,                  # répétition pour cross-validation
  DataSplit = 70,                  # 70% train / 30% test
  VarImport = 0,                   # pas de permutation pour l'instant
  models.eval.meth = c('TSS', 'ROC'),
  SaveObj = TRUE,
  rescal.all.models = FALSE
)

### Evaluation results
get_evaluations(biomod_model)

### Ensemble model (average over runs)
biomod_ensemble <- BIOMOD_EnsembleModeling(
  modeling.output = biomod_model,
  chosen.models = 'all',
  em.by = 'all',
  eval.metric = 'TSS',
  prob.mean = TRUE,
  prob.cv = FALSE,
  prob.ci = FALSE
)

### Projection on present climate
# Raster stack présent
clim_present <- rast("data/climate/present/*.tif")
names(clim_present) <- env_vars

biomod_proj_present <- BIOMOD_Projection(
  modeling.output = biomod_model,
  new.env = clim_present,
  proj.name = "present",
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = TRUE
)

### Projection on future climate
clim_future <- rast("data/climate/future/*.tif")
names(clim_future) <- env_vars

biomod_proj_future <- BIOMOD_Projection(
  modeling.output = biomod_model,
  new.env = clim_future,
  proj.name = "future",
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = TRUE
)

### Ensemble projection (mean across models)
biomod_ensemble_present <- BIOMOD_EnsembleForecasting(
  EM.output = biomod_ensemble,
  projection.output = biomod_proj_present,
  selected.models = 'all',
  binary.meth = 'TSS',
  prob.mean = TRUE
)

biomod_ensemble_future <- BIOMOD_EnsembleForecasting(
  EM.output = biomod_ensemble,
  projection.output = biomod_proj_future,
  selected.models = 'all',
  binary.meth = 'TSS',
  prob.mean = TRUE
)

### Save projections
writeRaster(rast(biomod_ensemble_present@proj@val), "outputs/SDM_present_biomod.tif", overwrite = TRUE)
writeRaster(rast(biomod_ensemble_future@proj@val), "outputs/SDM_future_biomod.tif", overwrite = TRUE)



