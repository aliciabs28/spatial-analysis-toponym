# DENSITY BASED APPROACH ON STREET NETWORK OF THE NON-USE OF TOPONYMS IN THE CITY
# USING SPNETWORK LIBRARY DEVELOPED BY J.GELB 2021
# CREATE IN 2022
# R

# IMPORT LIB
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


## IMPORT ADS ON STREETS NETWORK : SPATIAL RELATIOSHIP OF SITUATION EXTRACTED
ADS2 <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/ALPES-MARITIMES/1_ADS_RELOCALIZE_IN_OK.rds"))
#ADS2 <- subset(ADS, new_cat_rs == "IN")
#ADS2 <- unique(ADS[, "idAnnonce", drop = FALSE])

## IMPORT STREETS
LIGNE <- st_read(dsn ="C:/GIT/2022/TOPONOMASCAPE/STREETS/STREETS_06_SELECTED_WITH_CITY.shp")
LIGNE$NOM_max = toupper(LIGNE$NOM_max)


##############################################################################################################
## PARTIE 1.

# SELECT CITY
VILLE = "LE CANNET"

#SELECT STREETS OF THE CITY
LIGNE_VILLE <- subset(LIGNE, NOM_max == VILLE)

#IMPORT LIXELS (PREVIOULSY) AND CREATE SAMPLE PTS TO PERFORM NETKDE
#lixels <- lixelize_lines(LIGNE_VILLE, 100, 50)
lixels <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_VILLE.rds"))
sample_pts <- lines_center(lixels)


## EXTRACT SUBSET OF EACH TOPONYM TO PERFORM NETKDE
# IMPORT JSON DICT (SOME XY FOR EACH TOPONYM)
json_data <- fromJSON(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/dict_ads_negatives.json"))

#SELECT DATA FROM DICT
df_list <- list() 
for (nom in names(json_data)) {
  subset_df <- ADS2[ADS2$idAnnonce %in% json_data[[nom]], ]
  df_list[[nom]] <- subset_df
}


## APPLYING FUNCTION AND CALCULATE NETKDE OF NON-USE OF TOPONYM

#Import dict of KNN to use same parameters
DICTIONNAIRE_KNN <- readRDS(paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/DICTIONNAIRE_KNN.rds"))
for (i in 1:length(df_list)) {
  subset_points <- df_list[[i]]
  print(names(df_list)[i])
  
  print("...............applying buffer")
  subset_buffer <- st_buffer(subset_points, dist = 800)
  subset_combined <- st_union(subset_buffer)
  lines_within_buffer = filter(LIGNE_VILLE, st_intersects(LIGNE_VILLE, subset_combined, sparse = F))
  
  #Apply NETKDE
  print("...............applying NETKDE")
  lixels[[names(df_list)[i]]]<- netkde(ligne = lines_within_buffer, myevents = subset_points, moyx2 = DICTIONNAIRE_KNN[[names(df_list)[i]]]*2)
  
  print(".............end...........")
}

#save as xlsx
write_xlsx(lixels,  paste0("C:/GIT/2022/TOPONOMASCAPE/VILLE/", VILLE, "/LIXELS_NETKDE_NEGATIF_0606.xlsx"))


##############################################################################################################
