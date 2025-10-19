#----------------------------------------------------------#
# Install and load packages -----
#----------------------------------------------------------#
packages <- c("tidyverse", "pdftools", "RCzechia")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

#----------------------------------------------------------#
# Load reference data -----
#----------------------------------------------------------#
# Borders of Czechia
czechia_border <- 
  RCzechia::republika(
    resolution = "high"
  ) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) 

# Data on protected areas and mapping fields
endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- base::paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_sitmap0rad <- "Opendata:Mapovaci_sit_-_zakladni_pole"

getfeature_url_sitmap0rad <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_sitmap0rad
)

sitmap <- sf::st_read(getfeature_url_sitmap0rad) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) %>%
  sf::st_crop(
    .,
    czechia_border  # crop by the border of Czechia
  )

#----------------------------------------------------------#
# Load occurrence data -----
#----------------------------------------------------------#
data_ndop <- 
  readr::read_csv2(
    "Data/Input/data_ndop.csv",
    locale = locale(encoding = "Windows-1250")
  )
