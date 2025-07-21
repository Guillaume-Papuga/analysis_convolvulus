#######
# Script Convolvulus lanuginosus
# Guillaume Papuga
#######

setwd("~/Documents/Recherche/Projets & articles/en cours/article_genetique & Convol lanu/data")
# Packages
require(dismo)
require(ade4)
library(sp)
library(raster)
library(rgdal)
library(XML)
library(lattice)
library(grid)
library(foreign)
library(rgeos)
library(maptools) ## checks «rgeos» availability (if so, it will be the option used)
library(jsonlite)
library(maps)
library(mapdata)


#######
# 1. Données localisation espèce (+pseudo absence)
#######

####
# 1.a. Telechargement des données
####
## Définir l'extent de la zone de travail
# First, we may need to call the world map included in «maptools»; then plot a map such as this:
data(wrld_simpl) # plot carte globale
plot(wrld_simpl, xlim=c(-5,10), ylim=c(30,55), axes=TRUE, col='light yellow') # cadre sur Med ouest
box() # restores the box around the map
eo <-drawExtent() # definir un extent spatial sur une carte

## Données perso
convolvulus=read.csv("convolvulus.csv",header=TRUE,row.names=NULL,dec=",",sep=";")
convolvulus=convolvulus[,c(3,2,1)]  # remet les colonnes dans l'ordre! Très important

## Données GBIF
#convolvulus.gbif = gbif("convolvulus", "lanuginosus", ext=eo, geo=T, download=T) ## if download = FALSE, the records will be shown but not downloaded
#points(convolvulus.gbif$lon, convolvulus.gbif$lat, col='red', pch=20, cex=0.4)

## Nettoyage des données
#dim(convolvulus.gbif)
[1] 2913   25
#colnames(convolvulus.gbif)
#lat35 = subset(palmito, lat<=35.5)
#tarifa = subset(palmito, adm2=="Tarifa")
#murcia = subset(palmito, adm1=="Mu")

## Collage des deux tableaux
# rbind

## Imprime le tableau final, pour éviter la repetition de ces étapes
#write.table(palmito, file = "C:/.../Palmito_dat_GBIF.txt",


## Validation de la carto
# palmito2 <-read.table("C:/.../Chamaerops_GBIF_modified.txt", dec=",", header=T, row.names=1)
## note that decimals are as "commas"
#attach(palmito2)
#dim(palmito2)

zone.etude <-map("world", regions=c("Spain",  "Portugal", "Andorra","France"), exact=TRUE,fill=T)  # défini le fond de carte en "objet map"; il faut que "fill = T" pour pouvoir convertir en spatialpolygon
IDs <- sapply(strsplit(zone.etude$names, ":"), function(x) x[1]) # récupère les noms des pays
zone.etudeSpp <- map2SpatialPolygons(zone.etude, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))  # convertit en "Spatial Polygon"
points(convolvulus$Longitude, convolvulus$Latitude, col='red', pch=20, cex=0.9) # affiche les points

####
# 1.b. Pseudo absence
####
## Méthode 1: utilise la fonction spsample [non utilisé ici]
# Pour utiliser la fonction spsample, il faut définir un objet "spatial polygon" 
random.Spoints<-spsample(zone.etudeSpp, 500, type="random") # spsample echantillonne dans un "SpatialPolygon
map(zone.etude) # présente carte vide
plot(random.Spoints,add=T,pch=1,cex=0.4) # affiche les points randoms

## Méthode 2: utilise la fonction randomPoints [non utilisé ici]
# Pour utiliser cette méthode, il faut faire appel à un raster en premier
# randomPoints (predictors, 500)

## Méthode 3 : pseudo-absence dans un cercle de 100km autour des psce [utilisé ici]
convolvulusSPDF = convolvulus

# Changer le dataframe en un "SpatialPointDataFrame"
coordinates(convolvulusSPDF) = ~Longitude+Latitude # set spatial coordinates to create a Spatial object
projection(convolvulusSPDF) = CRS('+proj=longlat +datum=WGS84') # set the coordinate reference system (CRS)

# circles with a radius of 100 km
x = circles(convolvulusSPDF, d=100000, lonlat=TRUE) # modele qui définit des zones circulaires autour de points
pol = polygons(x) # sets spatial coordinates to create spatial data
pol.land = intersect( pol, zone.etudeSpp) # intercept selectionne juste la zone sur terre (evite de sampler dans la mer)

# random sample of points within the polygons with one point per grid cell
# sample randomly from all circles
samp1 <- spsample(pol.land, 1250, type='random', iter=25)

# choisit un raster qu'on utilise pour ne selectionner qu'un point par "cell"
setwd("~/Documents/Recherche/Ressources recherche/Données brutes/Bioclim/current")
mask<-crop(raster("bio_1"),eo) # prends bio_1 au pif
setwd("~/Documents/Recherche/Projets & articles/en cours/article_genetique & Convol lanu/data")

# get unique cells
cells <- cellFromXY(mask, samp1) # cellFrom permet de récuperer les numéros des cellules avec toute sorte de fichier (XY, ligne, polygone, etc.)
length(cells)
cells <- unique(cells)
length(cells)

# enlever les valeurs doublons avec les cellules contenant l'espèce
cells.p <- cellFromXY(mask, convolvulus [,1:2]) # récupérer les cellules de présence
cells.p = unique(cells.p) 
cells = setdiff(cells, cells.p) # enlève les points communs aux deux
length (cells)
xy <- xyFromCell(mask, cells) # récupère les coordonnées des cellules de mask dont le n° correspond a cell

random.Spoints = data.frame(xy) # reprend le mm nom que sur méthode avant pour pseudo absence
coordinates(random.Spoints) = ~x+y # set spatial coordinates to create a Spatial object
projection(random.Spoints) = CRS('+proj=longlat +datum=WGS84') # set the coordinate reference system (CRS)


# representation graphique
plot(pol.land, axes=TRUE)
plot(wrld_simpl, add=TRUE, border='dark grey')
points(xy, cex=0.1, pch=20, col='blue')
xy.p = convolvulusSPDF
points(xy.p, cex=0.01, pch=20, col='red')

####
# 1.c. Combiner les deux
####
bg=as.data.frame(cbind(random.Spoints$x,random.Spoints$y,rep(0,length(random.Spoints$x))))  # extrait les background points
colnames(bg)=c("x","y","psce_abs")  # donne les noms de colonnes

psce=as.data.frame(cbind(convolvulus$Longitude, convolvulus$Latitude,rep(1,length(convolvulus$Latitude))))
colnames(psce)=c("x","y","psce_abs")  # donne les noms de colonnes

p_a_convolvulus=rbind(bg,psce)  # colle les deux tableaux

#######
# 2. Donnés Worldclim 
#######
files=list.files(path = '/Users/guillaumepapuga/Documents/Recherche/Ressources recherche/Données brutes/Bioclim', pattern='grd',full.names=T)

####
# 2.a. Telecharger les données worldclim (et autre)
####
## Télécharger avec le système de Raster ESRI (présent sur l'ordi)
setwd("~/Documents/Recherche/Ressources recherche/Données brutes/Bioclim/current")
for (i in 1:19) {
  r<-raster(paste("bio_",i,sep=""))
  r.crop=crop(r, eo)
  assign(x=paste("r.crop_",i,sep=""),value=r.crop)
  }
predictors=stack(r.crop_1,r.crop_2,r.crop_3,r.crop_4,r.crop_5,r.crop_6,r.crop_7,r.crop_8,r.crop_9,r.crop_10,r.crop_11,r.crop_12,r.crop_13,r.crop_14,r.crop_15,r.crop_16,r.crop_17,r.crop_18,r.crop_19)
setwd("~/Documents/Recherche/Projets & articles/en cours/article_genetique & Convol lanu/data")


# !!!!!!! Toujours faire attention a ce que l'ensemble des points (bg + psce) soient dans l'area du predictor... sinon gros merdier !!!!!!!!!!!!!!

## Télécharger avec le système de raster GRID (à downloader du net..)

####
# 2.b. Selectionner les variables pour enlever la colinéarité (PCA)
####
bg.ext = as.data.frame(na.omit(extract(predictors,eo,cellnumbers=T))) # extrait toutes les valeurs des predicteurs dans une table, en gardant les numéros de cellule
cell.area = unlist(cellFromPolygon(mask,pol.land)) # on récupère les numéros de cellules dans pol.land
            # on utilise "unlist" pour réunir en un vecteur les valeurs de cellule des différents pays (séparés par la fonction)
bg.area = na.omit (bg.ext [which(bg.ext$cell %in% cell.area),]) # on garde les lignes de bg ayant un numéro de cellule compris dans cell.total
xy.t <- xyFromCell(mask, bg.area$cell) # récupère les xy de l'ensemble des cellules de la zone

points(xy.t,cex=0.001, pch=20, col='grey') # on plot sur le graph précédent

## PCA
pca.area = dudi.pca(bg.area[,2:20], scannf = F, nf = 2)
s.corcircle (pca.area$co)

var.retained = c("bio_1","bio_2","bio_4","bio_5","bio_12","bio_16") # liste des variables à retenir


## Contrles et visualisation
plot(predictors) # carte de tous les rasters
names(predictors)
plot(predictors$bio_3)

## Carte un peu plus complète avec lien variable/psce
data("wrld_simpl") # récupère les limites du monde 
plot(predictors$bio_2) # affiche le raster choisi (ici le bio_2)
plot(wrld_simpl,add=T) # ajoute les countours des pays
points(convolvulus$Longitude,convolvulus$Latitude,col='red', pch=20, cex=0.9)  # ajoute les points

####
# 2.b. Extraire les données
####

## utilise la fonction extract
val.bioclim = extract(predictors,p_a_convolvulus[,1:2],cellnumbers=F) # extrait les valeurs
val.bioclim = val.bioclim [,var.retained]
val.bioclim=cbind(p_a_convolvulus$psce_abs,val.bioclim) # lie avec psce absence
val.bioclim=na.omit(val.bioclim) # enlève les NA si l'extent du sample de BG points est different de l'extent du raster...)
colnames(val.bioclim)[1]="psce_abs"
summary(val.bioclim)

## enlève les doublons (même cellule) pour présence et absence

# et on enlève la colonne "cell"

## analyse de la correlation dans les data
pairs(val.bioclim[,2:7],cex=0.1,fig=T)



#######
# 3. Modelisation 
#######

## Crée jeu de données training et test pour les données présence
group=kfold(convolvulus,5) # crée 5 groupes au hasard
pres_train=convolvulus[group !=1,1:2] # selectionne tous les points sauf ceux du groupe 1
pres_test=convolvulus[group ==1,1:2] # selectionne que les points du groupe 1

## Crée jeu de données training et test pour les données bacground
group=kfold(bg,5) # crée 5 groupes au hasard
back_train=bg[group !=1,1:2] # selectionne tous les points sauf ceux du groupe 1
back_test=bg[group ==1,1:2] # selectionne que les points du groupe 1

## Plot pour visualiser les groupes
par(mfrow=c(1,2))
# Graph "train"
plot(pol.land, axes=TRUE, main = "Train")
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train,cex=0.1, pch=20, col='red')
points(back_train,cex=0.1, pch=20, col='blue')

# Graph "test"
plot(pol.land, axes=TRUE, main = "Test")
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_test,cex=0.1, pch=20, col='orange')
points(back_test,cex=0.1, pch=20, col='blue')

par(mfrow=c(1,1)) # ferme la fenetre

################# Presence only ####################################

# on restreint "predictors" aux variables voulues
predictors=stack(r.crop_1, r.crop_2, r.crop_4, r.crop_5, r.crop_12, r.crop_16)

####
# 3.b. Bioclim (package Dismo)
####
# Modèle en présence seul implémenté dans Dismo
bc = bioclim(predictors,pres_train)  # fit le modèle
e.b = evaluate(pres_test,back_test,bc,predictors) # evalue le modèle, avec un jeu de psce et un jeu d'absence (fait une matrice de confusion)
e.b # valeur de l'évaluation
tr.b = threshold(e.b,'spec_sens')  # fixe un seuil pour passer en binaire, sur un élément "evaluate"
tr.b # valeur du seuil

pb = predict(predictors,bc,progress='') # prediction stockée sous forme de Raster Layer
plot(pb, main="Bioclim, raw values") # plot le raster
plot(pb>tr.b,main="Presence/absence") # plot en binaire

####
# 3.c. Domain
####
dm=domain(predictors,pres_train)
e.d =evaluate(pres_test,back_test,dm,predictors)
pd=predict(predictors,dm,progress='')
tr.d =threshold(e.d,'spec_sens')
plot(pd, main="Domain, raw values")
plot(pd>tr.d,main="Presence/absence")

####
# 3.d. Mahalanobis
####

mm = mahal(predictors,pres_train)
e.m =evaluate(pres_test,back_test,mm,predictors)
pm = predict(predictors, mm, ext=eo, progress='') 
## Fail 
tr.m =threshold(e.m,'spec_sens')
par(mfrow=c(1,2))

pm[pm < -10] <- -10
plot(pm, main='Mahalanobis distance')
plot(wrld_simpl, add=TRUE, border='dark grey') > tr <- threshold(e, 'spec_sens')
plot(pm > tr.m, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey') > points(pres_train, pch='+')


################# Presence / Absence ####################################
# on fait tourner les analyses avec val.bioclim
val.bioclim=as.data.frame(val.bioclim)

####
# 3.a. GLM
####
m1 = glm(psce_abs~., data = val.bioclim)
m2 = step(object=m1,direction = "both" )
summary(m2)
p.m2 = predict(crop(predictors, eo),m2)
e.m2 = evaluate(pres_test,back_test,m2,predictors)
tr.m2 = threshold (e.m2,'spec_sens')
plot(p.m2)
plot(p.m2 > tr.m2)


####
# 3.e. Generalized Additive Model
####
require(mgcv)
colnames (val.bioclim)
gm = gam(psce_abs ~ bio_1 + bio_2 + bio_4 + bio_5 + bio_12 + bio_16, data = val.bioclim)  # choix des variables (pas d'utilisation du  ~ .)
summary(gm)
p.gm = predict(crop(predictors, eo),gm)
e.gm = evaluate(pres_test, back_test, gm, predictors)
tr.gm = threshold (e.gm,'spec_sens')
plot(p.gm)
plot(p.gm>tr.gm)

####
# 3.f. Maxent (Machine Learning)
####
Sys.setenv(NOAWT=TRUE)
library(rJava)
# xm = maxent(predictors, na.omit(pres_train),a=NULL, factors=NULL, removeDuplicates=TRUE, nbg=10000)
# plot(xm)

####
# 3.f. Maxent with R bis(Machine Learning)
####

####
# 3.g. Boosted Regression Trees
####
require(gbm)
# cherche le bon nombre de Trees
brt.step <- gbm.step(data=val.bioclim, # jeu de données
                gbm.x = 2:7, # variables explicatives
                gbm.y = 1,  # variable réponse (a expliquer)
                family = "bernoulli", # famille de modèle
                tree.complexity = 5, 
                learning.rate = 0.01, 
                bag.fraction = 0.5)

nb.trees = 3400 # XXXX Trees
brt = gbm.fixed(data = val.bioclim, gbm.x = 2:7, gbm.y = 1, tree.complexity = 1, n.trees = nb.trees) 
p.brt = predict(crop(predictors, eo),brt, n.trees = nb.trees)
e.brt = evaluate(pres_test, back_test, brt, predictors, n.trees = nb.trees)
tr.brt = threshold (e.brt,'spec_sens')
plot(p.brt)
plot(p.gm>tr.brt)

####
# 3.h. Random forest
####
library(randomForest)
## Randomforest fait soit une regression linéaire simple (variable numérique) 
# soit une classification (variable catégorielle). On doit choisir entre 
# changer la variable '0-1' de psce_abs en factor (avec la fonction factor() )
# ... sinon regression!

rf1 <- randomForest(psce_abs ~ bio_1 + bio_2 + bio_4 + bio_5 + bio_12 + bio_16, data = val.bioclim)  
# rf2 <- randomForest(envtrain[,1:8], factor(pb_train)) # on peut spécifier le modèle sans utiliser une formule
e.rf <- evaluate(pres_test,back_test,rf1,predictors)
e.rf
tr.rf <- threshold(e.rf, 'spec_sens')
p.rf1 <- predict(predictors, rf1, ext = eo)
par(mfrow=c(1,2))
plot(p.rf1, main='Random Forest, regression') # plot le résultat du modele
plot(wrld_simpl, add=TRUE, border='dark grey') # plot les contours du monde
plot(p.rf1 > tr.rf, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey') 
par(mfrow=c(1,1))

####
# 3.i. Support Vector Machine
####

require (kernlab)
svm = ksvm (psce_abs ~ bio_1 + bio_2 + bio_4 + bio_5 + bio_12 + bio_16, data = val.bioclim)
e.svm = evaluate(pres_test, back_test, svm, predictors)
p.svm <- predict(predictors, svm, ext=eo)
tr.svm <- threshold(e.svm, 'spec_sens')

par(mfrow=c(1,2))
plot(p.svm, main='Support Vector Machine')
plot(wrld_simpl, add=TRUE, border='dark grey') 
plot(p.svm > tr.svm, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
dev.off()

#######
# 4. Evaluation of models  (Machine learning)
#######
# analyse de la fonction "evaluate"

#######
# 5. Comparing modele output & model averaging
#######

## Simple representation
models = stack (pb, pd, p.m2, p.gm, p.brt, p.rf1, p.svm) # on reunit les raster associés aux predictions des modèles
names (models) = c("bioclim", "domain", "glm", "gam", "BRT", "randomforest", "SVM")
plot (models)


## Model averaging
gen.m = mean (models) # !!! attention, les modèles ne sont pas tous sur la même échelle...
plot (gen.m)

# avec correction
auc = sapply(list(e.b, e.d, e.m2, e.gm, e.brt, e.rf, e.svm), function(x) x@auc) # les éléments de 'evaluate' sont liés avec un @
w <- (auc-0.5)^2  # enlève 0.5 (deviation de random) et passe au carré pour appuyer les modele qui prédisent bien
gen.m2 <- weighted.mean( models[[c("bioclim", "domain", "glm", "gam", "BRT", "randomforest", "SVM")]], w) 
plot(gen.m2, main='weighted mean of three models')
plot(gen.m2 > 0.5, main='weighted mean of three models')




#######
# 6. Geographical methods
#######


# Spatial Kernel
## An example of spatial segregation analysis
## Not run:
## source in Lansing Woods tree data within a polygon boundary
data(lansing)
data(polyb)
## select data points within polygon
ndx <- which(pinpoly(polyb, as.matrix(lansing[c("x", "y")])) > 0)
pts <- as.matrix(lansing[c("x", "y")])[ndx,]
marks <- lansing[["marks"]][ndx]
## select bandwidth
h <- seq(0.02, 0.1, length=101)
cv <- cvloglk(pts, marks, h=h)$cv
hcv <- h[which.max(cv)]
plot(h, cv, type="l")
## estimate type-specific probabilities and do segregation tests
## by one integrated function
sp <- spseg(pts, marks, hcv, opt=3, ntest=100, poly=polyb)
## plot estimated type-specific probability surfaces
plotphat(sp)
## additional with pointwise significance contour lines
plotmc(sp, quan=c(0.025, 0.975))
## p-value of the Monte Carlo segregation test
cat("\np-value of the Monte Carlo segregation test", sp$pvalue)
##estimate intensity function at grid point for presentation
##with bandwidth hcv
gridxy <- as.matrix(expand.grid(x=seq(0, 1, length=101), y=seq(0, 1, length=101)))
ndx <- which(pinpoly(polyb, gridxy) > 0) ##inside point index
lam <- matrix(NA, ncol=101, nrow=101)
lam[ndx] <- lambdahat(pts, hcv, gpts = gridxy[ndx,], poly =
                        polyb)$lambda
brks <- pretty(range(lam, na.rm=TRUE), n=12)
plot(0, 0, xlim=0:1, ylim=0:1, xlab="x", ylab="y", type="n")
image(x=seq(0, 1, length=101), y=seq(0, 1, length=101),
      z=lam, add=TRUE, breaks=brks, col=risk.colors(length(brks)-1))
polygon(polyb)
metre(0, 0.01, 0.05, 0.51, lab=brks, col=risk.colors(length(brks)-1), cex=1)
## An example of inhomogeneous intensity function and K function
## estimated with the same data
s <- seq(0, 0.06, length=101)
lam <- lambdahat(pts, hcv, poly=polyb)$lambda
kin <- kinhat(pts, lam, polyb, s)
plot(kin$s, kin$k-pi*(kin$s)^2, xlab="s", ylab="k-pi*s^2", type="l")
## End(Not run)



#######
# 7. Code BIOMOD Sam Pironon
#######
library(biomod2)

# load("~/Documents/CEFE/Narcissus/Species/Nar_dub")
# DataSP[364:length(DataSP[,3]),3]<-NA

# load("~/Documents/CEFE/Narcissus/Proj/Current")
# load("~/Documents/CEFE/Narcissus/Proj/PastLGM_CCSM")
# load("~/Documents/CEFE/Narcissus/Proj/PastLGM_MIROC")

# setwd("~/Documents/CEFE")

####
# 7.a. formater les datas 
####
## 1. en utilisant un jeu de données p.a + un rasterstack
# la fonction extrait les données du rasterstack
myBiomodData <- BIOMOD_FormatingData(resp.var = p_a_convolvulus[,3], 
                                     expl.var = predictors, 
                                     resp.xy = p_a_convolvulus[,1:2], 
                                     resp.name = colnames(p_a_convolvulus)[3])
plot(myBiomodData) # distribution des points

## 2. en utilisant un jeu de données p.a + les données extraites
tab.comp = cbind (p_a_convolvulus,val.bioclim [,-1])
tab.comp = na.omit (tab.comp)
myBiomodData <- BIOMOD_FormatingData(resp.var = tab.comp[,3], 
                                     expl.var = tab.comp[4:9], 
                                     resp.xy = tab.comp[,1:2], 
                                     resp.name = colnames(tab.comp)[3])
                                     
plot(myBiomodData) # distribution des points

## 3. en utilisant un jeu de données presence only + simulant les absences avec BIOMOD2
psce.only = tab.comp [which(tab.comp$psce_abs==1),1:3]
myBiomodData <- BIOMOD_FormatingData(resp.var = psce.only[,3], 
                                     expl.var = predictors, 
                                     resp.xy = psce.only[,1:2], 
                                     resp.name = colnames(psce.only)[3],
                                     PA.nb.rep = 1,
                                     PA.nb.absences = 100,
                                     PA.strategy = 'random')
                                     
plot(myBiomodData) # distribution des points

####
# 7.b. building model 
####
## customize their set of parameters and options using BIOMOD_ModelingOptions
# here default option
myBiomodOption <- BIOMOD_ModelingOptions()

## fit the model
myBiomodModelOut <- BIOMOD_Modeling( myBiomodData, 
                                     models = c(
                                       # 'GLM' # 
                                       # , 'GBM' 
                                       # , 'GAM'
                                         'CTA' 
                                       # , 'ANN' 
                                        , 'SRE' 
                                        , 'FDA' 
                                        , 'MARS'
                                       , 'RF'
                                       # , 'MAXENT'
                                       ), 
                                     models.options = myBiomodOption, 
                                     NbRunEval = 3, 
                                     DataSplit = 80, 
                                     Prevalence = 0.5, 
                                     VarImport = 3, 
                                     models.eval.meth = c('TSS','ROC'), 
                                     SaveObj = TRUE, 
                                     rescal.all.models = TRUE, 
                                     do.full.models = FALSE, 
                                     modeling.id = paste(colnames(psce.only)[3],"FirstModeling",sep=""))
myBiomodModelOut

####
# 7.c. evaluation of the model 
####
# get all models evaluation
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
# print the dimnames of this object
dimnames(myBiomodModelEval)
myBiomodModelEval

# let's print the TSS scores of Random Forest
myBiomodModelEval["TSS","Testing.data","RF",,]

# let's print the ROC scores of all selected models
myBiomodModelEval["ROC","Testing.data",,,]

# print variable importances
get_variables_importance(myBiomodModelOut)


####
# 7.d. Combining models
####
## BIOMOD_EnsembleModeling combines individual models to build some kind of meta-model

myBiomodEM <- BIOMOD_EnsembleModeling (modeling.output = myBiomodModelOut, 
                                       chosen.models = 'all', 
                                       em.by='all', 
                                       eval.metric = c('TSS'), 
                                       eval.metric.quality.threshold = c(0.6), 
                                       prob.mean = F, 
                                       prob.cv = F, 
                                       prob.ci = F, 
                                       prob.ci.alpha = 0.05, 
                                       prob.median = F, 
                                       committee.averaging = F, 
                                       prob.mean.weight = T, 
                                       prob.mean.weight.decay = 'proportional' )

####
# 7.e. Evaluation du modele moyen
####
# We decide to evaluate all meta-models produced even the CV (Coefficient of Variation)
# one which is quite hard to interpret. You may consider it as: higher my score is, 
# more the variation is localised where my species is forecasted as present.

# print summary
myBiomodEM
# get evaluation scores
get_evaluations(myBiomodEM)


####
# 7.e. Project du modèle moyen
####

## données actuelles (utilisé dans la définition du modèle) avec modèles individuels
myExpl = predictors
myBiomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                  new.env = myExpl, 
                                  proj.name = 'current', 
                                  selected.models = 'all', 
                                  binary.meth = 'TSS', 
                                  compress = 'xz', 
                                  clamping.mask = F, 
                                  output.format = '.grd')

myBiomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut, new.env = Current, proj.name = 'Current', selected.models = 'all', binary.meth = 'TSS', compress = 'xz', clamping.mask = F, output.format = '.grd')

## ensemble forecasting
# project the meta-models you have created with BIOMOD_EnsembleModeling
myBiomodEF <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM,
  projection.output = myBiomodProj)

## données passées CCSM
myBiomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut, new.env = PastLGM_CCSM, proj.name = 'PastLGM_CCSM', selected.models = 'all', binary.meth = 'TSS', compress = 'xz', clamping.mask = F, output.format = '.grd')

## données passées MIROC
myBiomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut, new.env = PastLGM_MIROC, proj.name = 'PastLGM_MIROC', selected.models = 'all', binary.meth = 'TSS', compress = 'xz', clamping.mask = F, output.format = '.grd')


##### Analyse CS
genet = read.csv("data.genet.csv", header = T, row.names = 1, dec = ",", sep = ";")


# Ho et He
vec = c (rbind (genet$He, genet$Ho))
barplot(vec, space = c(0, rep (c(1, 2), 2), 1,
                       7, rep (c(1, 2), 2), 1, 
                       7, rep (c(1, 2), 7), 1, 
                       7, rep (c(1, 2), 3), 1), 
        col = c (rep (c("red", "lightpink1"), 3), 
                 rep (c("blue", "skyblue"), 3),
                 rep (c("purple", "plum2"), 8),
                 rep (c("darkgreen", "lightgreen"), 4)))
        
abline (h = 0.5, lty = 3)
abline (h = 0)


plot(genet$He, genet$Ho, 
     xlim = c(0.3, 0.8), ylim = c(0.3, 0.8), 
     xlab = "Expected heterozy", ylab = "Observed hetero")
abline(a = 0, b = 1)

# Fis
barplot(genet$Fis, space = c (0, 0.5, 0.5, 3, 0.5, 0.5, 3, rep (0.5, 7), 3, 0.5, 0.5, 0.5), 
        col = c(rep ("red", 3), rep ("blue", 3), rep ("purple", 8), rep ("green", 4)))
abline (h = 0)
abline (h = 0.2, lty = 3)


# PCA microniche
mat = matrice [which (matrice$espece== "Convolvulus lanuginosus"),]
grp.genet = as.vector (unique(mat$pop))
grp.genet = as.data.frame (cbind (grp.genet, 
                                  as.vector(c("a", "b", "b", "b", "c", "c", "b", "b", "b", "b"))))

mat = merge (mat, grp.genet, by.x = "pop", by.y = "grp.genet") # merge avec les groupes

pca.genet = dudi.pca (mat [, 5:24])
s.label (pca.genet$li)

s.class (pca.genet$li, fac = mat$pop, grid = F, cstar = 0)
s.class (pca.genet$li, fac = mat$posi, grid = F)
s.class (pca.genet$li, fac = mat$V2, grid = F)








