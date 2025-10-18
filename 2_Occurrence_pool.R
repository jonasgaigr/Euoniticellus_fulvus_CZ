#----------------------------------------------------------#
# Load processed occurrence data -----
#----------------------------------------------------------#

occ_mert_2020 <- 
  readr::read_csv(
    "Outputs/Data/nalezy_clean_Mertlik_2020_Elateridarium.csv"
  ) %>%
  dplyr::mutate(
    species = Druh,
    field = Ctverec,
    year = lubridate::year(Datum)
  ) %>%
  dplyr::select(
    species,
    field,
    year
  ) %>%
  dplyr::mutate(
    source = "mertlik_2020"
  )

occ_mert_2021 <- 
  readr::read_csv(
    "Outputs/Data/nalezy_clean_Mertlik_2021_Elateridarium.csv"
  ) %>%
  dplyr::mutate(
    species = Druh,
    field = Ctverec,
    year = lubridate::year(Datum)
  ) %>%
  dplyr::select(
    species,
    field,
    year
  ) %>%
  dplyr::mutate(
    source = "mertlik_2021"
  )

occ_ndop <-
  data_ndop %>%
  dplyr::mutate(
    species = DRUH,
    field = SITMAP,
    year = lubridate::year(DATUM_OD)
  ) %>%
  dplyr::select(
    species,
    field,
    year
  ) %>%
  dplyr::mutate(
    source = "nca_data"
  )

#----------------------------------------------------------#
# Combine processed occurrence data -----
#----------------------------------------------------------#


#----------------------------------------------------------#
# Sort symbology -----
#----------------------------------------------------------#