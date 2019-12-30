# Change this to your local path
setwd("/Users/clement/Documents/IRIS viz")

library(sf)
library(mapview)

# Source: https://www.insee.fr/fr/statistiques/4217503
income <- read.csv("income-iris-2015.csv")

# Source: https://geoservices.ign.fr/documentation/diffusion/telechargement-donnees-libres.html#contoursiris
# Make sure you download the contours from the right year as specified in the IRIS income data Excel
# In our case it was using the geography from 2016.
iris_map <- st_read("/Users/clement/Documents/IRIS viz/CONTOURS-IRIS_2-1__SHP__FRA_2017-06-30/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2016/CONTOURS-IRIS_2-1_SHP_LAMB93_FE-2016/CONTOURS-IRIS.shp")

# Pre-processing - extract Zip codes
iris_map$ZIP <- as.numeric(as.character(iris_map$INSEE_COM))
iris_map <- iris_map[-which(is.na(iris_map$ZIP)),]

# Pre-processing - Create Paris and Petite Couronne datasets
iris_map_paris <- iris_map[iris_map$ZIP >= 75000 & iris_map$ZIP < 76000,]
iris_map_petite_couronne <- iris_map[
  iris_map$ZIP >= 75000 & iris_map$ZIP < 76000 |
    iris_map$ZIP>=92000 & iris_map$ZIP < 95000
  ,]

# Pre-processing - augment geometries with income data
iris_map_paris_augmented <- merge(
  iris_map_paris,
  income[,c("IRIS", 
            "DEC_D115",
            "DEC_D215",
            "DEC_D315",
            "DEC_D415",
            "DEC_MED15",
            "DEC_D615",
            "DEC_D715",
            "DEC_D815",
            "DEC_D915")],
  by.x="CODE_IRIS",
  by.y="IRIS",
  all.x=TRUE)

iris_map_petite_couronne_augmented <- merge(
  iris_map_petite_couronne,
  income[,c("IRIS", 
            "DEC_D115",
            "DEC_D215",
            "DEC_D315",
            "DEC_D415",
            "DEC_MED15",
            "DEC_D615",
            "DEC_D715",
            "DEC_D815",
            "DEC_D915")],
  by.x="CODE_IRIS",
  by.y="IRIS",
  all.x=TRUE)

# Plot basic maps
plot(
  iris_map_paris_augmented[,"DEC_MED15"],
  main="Median income by consumption unit in 2015 (Euro)",
  lwd=0.2)

plot(
  iris_map_paris_augmented[,"DEC_D915"],
  main="9th decile income by consumption unit in 2015 (Euro)",
  lwd=0.2)

plot(
  iris_map_petite_couronne_augmented[,"DEC_MED15"],
  main="Median income by consumption unit in 2015 (Euro)",
  lwd=0.2)

# Pre-processing - Create lists for nice interactive plots
data_to_plot_paris = list(
  iris_map_paris_augmented[,"DEC_D115"],
  iris_map_paris_augmented[,"DEC_D215"],
  iris_map_paris_augmented[,"DEC_D315"],
  iris_map_paris_augmented[,"DEC_D415"],
  iris_map_paris_augmented[,"DEC_MED15"],
  iris_map_paris_augmented[,"DEC_D615"],
  iris_map_paris_augmented[,"DEC_D715"],
  iris_map_paris_augmented[,"DEC_D815"],
  iris_map_paris_augmented[,"DEC_D915"])

data_to_plot_petite_couronne = list(
  iris_map_petite_couronne_augmented[,"DEC_D115"],
  iris_map_petite_couronne_augmented[,"DEC_D215"],
  iris_map_petite_couronne_augmented[,"DEC_D315"],
  iris_map_petite_couronne_augmented[,"DEC_D415"],
  iris_map_petite_couronne_augmented[,"DEC_MED15"],
  iris_map_petite_couronne_augmented[,"DEC_D615"],
  iris_map_petite_couronne_augmented[,"DEC_D715"],
  iris_map_petite_couronne_augmented[,"DEC_D815"],
  iris_map_petite_couronne_augmented[,"DEC_D915"])

# Plot interactive map
mapview(
  data_to_plot_petite_couronne,
  layer.name=c("D1", "D2", "D3", "D4", "Median", "D6", "D7", "D8", "D9"),
  map.types=c("CartoDB.Positron", "Esri.WorldImagery"),
  hide=TRUE,
  popup=NULL,
  homebutton=FALSE
  )
