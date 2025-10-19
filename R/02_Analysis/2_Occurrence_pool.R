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
    year = as.numeric(stringr::str_sub(DATUM_OD, 1, 4))
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
data_comb <-
  dplyr::bind_rows(
    occ_mert_2020,
    occ_mert_2021,
    occ_ndop
  ) %>%
  dplyr::filter(
    species == "Euoniticellus fulvus"
  ) %>%
  dplyr::group_by(
    species,
    field
  ) %>%
  dplyr::reframe(
    max_year = max(year, na.rm = TRUE),
    min_year = min(year, na.rm = TRUE),
    n_occ = n(),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct()

#----------------------------------------------------------#
# Sort symbology -----
#----------------------------------------------------------#

#--------------------------------------------------#
# 3) Get Czech 1×1 km grid or “čtverce” polygons
#--------------------------------------------------#
# RCzechia::grids_cz() provides multiple grid levels, e.g. 1km, 10km
# For faunistic mapping, 10×10 km grid (“KFME”) is usually used
cz_grid <- sitmap  # 10 km grid

#--------------------------------------------------#
# 4) Join your data to the grid
#--------------------------------------------------#
map_data <- cz_grid %>%
  dplyr::left_join(
    .,
    data_comb,
    by = c("POLE" = "field")   # replace 'fid' by the field identifier column name in cz_grid
  ) %>%
  dplyr::filter(is.na(species) == FALSE)

map_data_hist <-
  map_data %>%
  dplyr::filter(
    min_year <= 1975
  ) %>%
  sf::st_simplify(
    ., 
    dTolerance = 100
  )

map_data_rec <-
  map_data %>%
  dplyr::filter(
    min_year > 1975
  ) %>%
  sf::st_simplify(
    ., 
    dTolerance = 100
    )


