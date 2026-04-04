library(spNetwork)
library(ggplot2)
library(tidyr)
library(spNetwork)
library(RColorBrewer)
library(kableExtra)
library(dplyr)
library(sp)
library(sf)
library(rgdal)
library(Rcpp)
library(RcppArmadillo)
library(writexl)
library(sf)
library(jsonlite)


#### IMPORT FUNCTION
netkde <- function(ligne, myevents, moyx2){
  nkde(lines = ligne,
       events = myevents,
       w = rep(1, nrow(myevents)),
       samples = sample_pts,
       kernel_name = "quartic",
       bw = moyx2, method = "simple",
       div = "bw", digits = 2, tol = 0.01,
       agg = 10, grid_shape = c(1, 1),
       verbose = FALSE)
}


## ANNONCES IMMOBILIERES RELOCALISEES SUR LE RESEAU : LES RELATIONS SPATIALES UNIQUEMENT "IN"
ADS2 <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/ALPES-MARITIMES/1_ADS_RELOCALIZE_IN_OK.rds"))
ADS2 <- subset(ADS, new_cat_rs == "IN")
ADS2 <- unique(ADS[, "idAnnonce", drop = FALSE])

## LIGNE DATA
LIGNE <- st_read(dsn ="C:/GIT/2022/TOPONOMASCAPE/STREETS/STREETS_06_SELECTED_WITH_CITY.shp")
LIGNE$NOM_max = toupper(LIGNE$NOM_max)


##############################################################################################################
## PARTIE 1.
VILLE = "LE CANNET"
LIGNE_VILLE <- subset(LIGNE, NOM_max == VILLE)
#IMPORT LIXELS 
#lixels <- lixelize_lines(LIGNE_VILLE, 100, 50)
lixels <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_VILLE.rds"))
sample_pts <- lines_center(lixels)


## EXTRAIRE LES SUBSETS POUR CHAQUE TOPONYME POUR ANALYSER LE NEGATIF
json_data <- fromJSON(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/dict_ads_negatives.json"))
df_list <- list() 
for (nom in names(json_data)) {
  # Sélectionner les lignes du dataframe qui ont des identifiants correspondants
  subset_df <- ADS2[ADS2$idAnnonce %in% json_data[[nom]], ]
  # Ajouter le sous-ensemble de données à la liste
  df_list[[nom]] <- subset_df
}


## APPLYING FUNCTION AND CALCULATE NETKDE ON NEGATIF
DICTIONNAIRE_KNN <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/DICTIONNAIRE_KNN.rds"))
for (i in 1:length(df_list)) {
  subset_points <- df_list[[i]]
  print(names(df_list)[i])
  
  print("...............applying buffer")
  # Créer un buffer autour des points
  subset_buffer <- st_buffer(subset_points, dist = 800)
  subset_combined <- st_union(subset_buffer)
  lines_within_buffer = filter(LIGNE_VILLE, st_intersects(LIGNE_VILLE, subset_combined, sparse = F))
  
  #Applying NETKDE
  print("...............applying NETKDE")
  lixels[[names(df_list)[i]]]<- netkde(ligne = lines_within_buffer, myevents = subset_points, moyx2 = DICTIONNAIRE_KNN[[names(df_list)[i]]]*2)
  
  print(".............end...........")
}

write_xlsx(lixels,  paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_NETKDE_NEGATIF_0606.xlsx"))


##############################################################################################################

lixels <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/",VILLE,"/LIXELS_VILLE.rds"))
NEGATIF = read_excel(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/",VILLE,"/NETKDE_RATIO.xlsx"))

library(readxl)


# Convertir le dataframe en objet sf avec des linestrings
sf_object <- st_as_sf(NEGATIF, wkt = "geometry")
st_write(sf_object, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/",VILLE,"/NETKDE_RATIO.shp"))



##############################################################################################################

lixels <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/NICE/LIXELS_VILLE.rds"))
NEGATIF = read_excel(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/GDF_1.xlsx"))



# Convertir le dataframe en objet sf avec des linestrings
sf_object <- st_as_sf(NEGATIF, wkt = "geometry")
st_write( df_list$bonaparte, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/NICE/bonaparte.shp"))


####
## PARTIE 1.
VILLE = "SAINT-LAURENT-DU-VAR"
LIGNE_VILLE <- subset(LIGNE, NOM_max == VILLE)
#IMPORT LIXELS 
#lixels <- lixelize_lines(LIGNE_VILLE, 100, 50)
lixels <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_VILLE.rds"))
sample_pts <- lines_center(lixels)


## EXTRAIRE LES SUBSETS POUR CHAQUE TOPONYME POUR ANALYSER LE NEGATIF
json_data <- fromJSON(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/dict_ads_negatives.json"))
df_list <- list() 
for (nom in names(json_data)) {
  # Sélectionner les lignes du dataframe qui ont des identifiants correspondants
  subset_df <- ADS2[ADS2$idAnnonce %in% json_data[[nom]], ]
  # Ajouter le sous-ensemble de données à la liste
  df_list[[nom]] <- subset_df
}


## APPLYING FUNCTION AND CALCULATE NETKDE ON NEGATIF
DICTIONNAIRE_KNN <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/DICTIONNAIRE_KNN.rds"))
for (i in 1:length(df_list)) {
  subset_points <- df_list[[i]]
  print(names(df_list)[i])
  
  print("...............applying buffer")
  # Créer un buffer autour des points
  subset_buffer <- st_buffer(subset_points, dist = 800)
  subset_combined <- st_union(subset_buffer)
  lines_within_buffer = filter(LIGNE_VILLE, st_intersects(LIGNE_VILLE, subset_combined, sparse = F))
  
  #Applying NETKDE
  print("...............applying NETKDE")
  lixels[[names(df_list)[i]]]<- netkde(ligne = lines_within_buffer, myevents = subset_points, moyx2 = DICTIONNAIRE_KNN[[names(df_list)[i]]]*2)
  
  print(".............end...........")
}

write_xlsx(lixels,  paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_NETKDE_NEGATIF_0606.xlsx"))

## PARTIE 1.
VILLE = "GRASSE"
LIGNE_VILLE <- subset(LIGNE, NOM_max == VILLE)
#IMPORT LIXELS 
#lixels <- lixelize_lines(LIGNE_VILLE, 100, 50)
lixels <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_VILLE.rds"))
sample_pts <- lines_center(lixels)


## EXTRAIRE LES SUBSETS POUR CHAQUE TOPONYME POUR ANALYSER LE NEGATIF
json_data <- fromJSON(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/dict_ads_negatives.json"))
df_list <- list() 
for (nom in names(json_data)) {
  # Sélectionner les lignes du dataframe qui ont des identifiants correspondants
  subset_df <- ADS2[ADS2$idAnnonce %in% json_data[[nom]], ]
  # Ajouter le sous-ensemble de données à la liste
  df_list[[nom]] <- subset_df
}


## APPLYING FUNCTION AND CALCULATE NETKDE ON NEGATIF
DICTIONNAIRE_KNN <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/DICTIONNAIRE_KNN.rds"))
for (i in 1:length(df_list)) {
  subset_points <- df_list[[i]]
  print(names(df_list)[i])
  
  print("...............applying buffer")
  # Créer un buffer autour des points
  subset_buffer <- st_buffer(subset_points, dist = 800)
  subset_combined <- st_union(subset_buffer)
  lines_within_buffer = filter(LIGNE_VILLE, st_intersects(LIGNE_VILLE, subset_combined, sparse = F))
  
  #Applying NETKDE
  print("...............applying NETKDE")
  lixels[[names(df_list)[i]]]<- netkde(ligne = lines_within_buffer, myevents = subset_points, moyx2 = DICTIONNAIRE_KNN[[names(df_list)[i]]]*2)
  
  print(".............end...........")
}

write_xlsx(lixels,  paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_NETKDE_NEGATIF_0606.xlsx"))

