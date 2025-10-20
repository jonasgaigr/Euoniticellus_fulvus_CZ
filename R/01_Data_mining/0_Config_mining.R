#----------------------------------------------------------#
# Install and load packages -----
#----------------------------------------------------------#
packages <- c("tidyverse", "pdftools", "RCzechia", "tidyterra", "raster",
              "ggnewscale", "ggrepel")

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
    st_crs("+init=epsg:4326")
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
    st_crs("+init=epsg:4326")
  ) %>%
  sf::st_filter(
    .,
    czechia_border  # crop by the border of Czechia
  )

# Load rayshaded hill
hill <- RCzechia::vyskopis("rayshaded", cropped = FALSE)

# Optionally crop to Czechia bounding box
czech_bbox <- czechia_border %>%
  sf::st_transform(
    .,
    st_crs("+init=epsg:4326")
    ) %>%
  st_buffer(., 5000) %>%
  st_bbox()

hill_cropped <- terra::crop(hill, czech_bbox)

# Reproject to 4326
hill <- terra::project(hill_cropped, "EPSG:4326")

rivers <- RCzechia::reky(resolution = "high") %>% 
  filter(Major)
  

#----------------------------------------------------------#
# Load occurrence data -----
#----------------------------------------------------------#
data_ndop <- 
  readr::read_csv2(
    "Data/Input/data_ndop.csv",
    locale = locale(encoding = "Windows-1250")
  )
