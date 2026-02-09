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
library(dplyr)

#### IMPORT FUNCTIONS
## function netkde
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
## function knn
distanceknn <- function(subset_points, LIGNE){
  results <- network_knn(subset_points, LIGNE,
                         k = nrow(subset_points), maxdistance = 10000, line_weight = "length", 
                         grid_shape=c(1,1), verbose = FALSE, tol = 0.1)  #k= len(pointsads)
  df <- as.data.frame(results[["distances"]])
  df <-df[!duplicated(df),,drop=F]
  df[df == 0] <- NA 
  df[df == Inf] <- NA 
  nouvelle_colonne <- apply(df, 1, function(x) x[match(TRUE, !is.na(x))])
  moyenne_nouvelle_colonne <- mean(nouvelle_colonne, na.rm = TRUE) 
  return(moyenne_nouvelle_colonne)
}

#### IMPORT DATA
ADS <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/ALPES-MARITIMES/1_ADS_RELOCALIZE_IN_OK.rds"))
LIGNE <- st_read(dsn ="C:/GIT/2022/TOPONOMASCAPE/STREETS/STREETS_06_SELECTED_WITH_CITY.shp")
LIGNE$NOM_max = toupper(LIGNE$NOM_max)



##############################################################################################################
## SELECT CITY 
VILLE = "ANTIBES"
LEN_MIN_TOPONYME = 20
# CREATION D'UN NOUVEAU DOSSIER VILLE
dir.create(file.path("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE))

# CREATE ADS & STREETS BY CITY 
ADS_VILLE <- subset(ADS, NOM_max == VILLE)
subset_list <- split(ADS_VILLE, ADS_VILLE$topo_3)
LIGNE_VILLE <- subset(LIGNE, NOM_max == VILLE)

#### LIXELS & SAMPLES PTS
lixels <- lixelize_lines(LIGNE_VILLE, 100, 50)
sample_pts <- lines_center(lixels)
saveRDS(lixels,  paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_VILLE.rds"))
st_write(lixels, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_VILLE.shp"))

# Tracer les points et les routes
#ggplot() +
 #geom_sf(data = ADS_VILLE, color = "red") +
 #geom_sf(data = lixels, color = "blue")

## DICTIONNAIRE KNN
DICTIONNAIRE_KNN <- list()  
for (i in 1:length(subset_list)) {
  subset_points <- subset_list[[i]]
  subset_name <- names(subset_list)[i]
  if (nrow(subset_points) >= LEN_MIN_TOPONYME) {
    print(subset_name)
    # calculate distance for current subset
    moyenne_knn <- distanceknn(subset_points, LIGNE_VILLE)
    # add key-value pair to dictionary
    DICTIONNAIRE_KNN[[subset_name]] <- moyenne_knn
  }
}
saveRDS(DICTIONNAIRE_KNN, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/DICTIONNAIRE_KNN.rds"))

# APPLYING NETKDE 
DICTIONNAIRE_KNN <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/DICTIONNAIRE_KNN.rds"))
for (i in 1:length(subset_list)) {
  subset_points <- subset_list[[i]]
  subset_name <- names(subset_list)[i]
  #if (nrow(subset_points) >= 50 & nrow(subset_points) < 100) {
  if (nrow(subset_points) >= LEN_MIN_TOPONYME) {
    print(subset_name)
    lixels[[subset_name]] <- netkde(ligne = LIGNE_VILLE, myevents = subset_points, moyx2 = DICTIONNAIRE_KNN[[subset_name]]*2)
  }
}
write_xlsx(lixels, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_NETKDE_POSITF.xlsx"))



##############################################################################################################
## SELECT CITY 
VILLE = "MENTON"
LEN_MIN_TOPONYME = 20
# CREATION D'UN NOUVEAU DOSSIER VILLE
dir.create(file.path("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE))

# CREATE ADS & STREETS BY CITY 
ADS_VILLE <- subset(ADS, NOM_max == VILLE)
subset_list <- split(ADS_VILLE, ADS_VILLE$topo_3)
LIGNE_VILLE <- subset(LIGNE, NOM_max == VILLE)

#### LIXELS & SAMPLES PTS
lixels <- lixelize_lines(LIGNE_VILLE, 100, 50)
sample_pts <- lines_center(lixels)
saveRDS(lixels,  paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_VILLE.rds"))
st_write(lixels, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_VILLE.shp"))

# Tracer les points et les routes
#ggplot() +
#geom_sf(data = ADS_VILLE, color = "red") +
#geom_sf(data = lixels, color = "blue")

## DICTIONNAIRE KNN
DICTIONNAIRE_KNN <- list()  
for (i in 1:length(subset_list)) {
  subset_points <- subset_list[[i]]
  subset_name <- names(subset_list)[i]
  if (nrow(subset_points) >= LEN_MIN_TOPONYME) {
    print(subset_name)
    # calculate distance for current subset
    moyenne_knn <- distanceknn(subset_points, LIGNE_VILLE)
    # add key-value pair to dictionary
    DICTIONNAIRE_KNN[[subset_name]] <- moyenne_knn
  }
}
saveRDS(DICTIONNAIRE_KNN, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/DICTIONNAIRE_KNN.rds"))

# APPLYING NETKDE 
DICTIONNAIRE_KNN <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/DICTIONNAIRE_KNN.rds"))
for (i in 1:length(subset_list)) {
  subset_points <- subset_list[[i]]
  subset_name <- names(subset_list)[i]
  #if (nrow(subset_points) >= 50 & nrow(subset_points) < 100) {
  if (nrow(subset_points) >= LEN_MIN_TOPONYME) {
    print(subset_name)
    lixels[[subset_name]] <- netkde(ligne = LIGNE_VILLE, myevents = subset_points, moyx2 = DICTIONNAIRE_KNN[[subset_name]]*2)
  }
}
write_xlsx(lixels, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_NETKDE_POSITF.xlsx"))






##############################################################################################################

#APPLY_NETKDE_POSITIF <- function(VILLE, LEN_MIN_TOPONYME, ADS, LIGNE){
  # CREATION D'UN NOUVEAU DOSSIER VILLE
  print(VILLE)
  dir.create(file.path("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE))
  
  # CREATE ADS & STREETS BY CITY 
  ADS_VILLE <- subset(ADS, NOM_max == VILLE)
  subset_list <- split(ADS_VILLE, ADS_VILLE$topo_3)
  LIGNE_VILLE <- subset(LIGNE, NOM_max == VILLE)

  print("...................Applying KNN")
  ## DICTIONNAIRE KNN
  DICTIONNAIRE_KNN <- list()  
  for (i in 1:length(subset_list)) {
    subset_points <- subset_list[[i]]
    subset_name <- names(subset_list)[i]
    if (nrow(subset_points) >= LEN_MIN_TOPONYME) {
      print(subset_name)
      # calculate distance for current subset
      moyenne_knn <- distanceknn(subset_points, LIGNE_VILLE)
      # add key-value pair to dictionary
      DICTIONNAIRE_KNN[[subset_name]] <- moyenne_knn
    }
  }
  saveRDS(DICTIONNAIRE_KNN, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/DICTIONNAIRE_KNN.rds"))
  
  print("...................Applying NETKDE")
  # APPLYING NETKDE 
  print(length(DICTIONNAIRE_KNN))
  for (i in 1:length(subset_list)) {
    subset_points <- subset_list[[i]]
    subset_name <- names(subset_list)[i]
    #if (nrow(subset_points) >= 50 & nrow(subset_points) < 100) {
    if (nrow(subset_points) >= LEN_MIN_TOPONYME) {
      print(subset_name)
      lixels[[subset_name]] <- netkde(ligne = LIGNE_VILLE, myevents = subset_points, moyx2 = DICTIONNAIRE_KNN[[subset_name]]*2)
    }
  }
  
  write_xlsx(lixels, paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_NETKDE_POSITF.xlsx"))
  
  print(".... END....")
}
#VILLE = "GRASSE"
#LEN_MIN_TOPONYME = 15
#APPLY_NETKDE_POSITIF_0(VILLE, LEN_MIN_TOPONYME, ADS, LIGNE)


