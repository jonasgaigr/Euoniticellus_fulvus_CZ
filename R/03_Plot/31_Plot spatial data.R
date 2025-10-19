
map <- ggplot() +
  #⃣ raster background
  tidyterra::geom_spatraster(data = hill) +
  scale_fill_gradientn(
    colors = scales::alpha(c("white", "grey90", "grey80","grey70", "grey60"), 0.3),  # light hillshade
    na.value = "white",
    guide = "none"
  ) +

  # Allow a new fill scale for points
  ggnewscale::new_scale_fill() +
  
  # Rivers
  geom_sf(data = rivers, color = "steelblue", size = 0.25) +
  
  # Czechia border
  geom_sf(data = czechia_border, fill = NA, color = "black", size = 0.5) +
  
  # Historical polygons (cross-hatch effect)
  geom_sf(data = map_data_hist, fill = "grey30", color = NA, alpha = 0.5) +
  
  # Recent points colored by period
  geom_sf(data = map_data_rec, aes(fill = max_year), color = "black", size = 0.15) +
  scale_fill_gradient2(
    name = "Rok prvního nálezu po roce 1975",
    low = "#4575B4", mid = "#FFFFBF", high = "#D73027",
    midpoint = 2015, na.value = "grey90"
  ) +
  
  # Coordinate grid
  coord_sf(crs = st_crs(4326), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.4),
    axis.text = element_text(size = 8)
  ) +
  labs(
    title = "Rozšíření Euoniticellus fulvus (Goeze, 1777)",
    subtitle = "Rozšíření před rokem 1975 šrafovaně, pozdější nálezy barevně",
    caption = "Data: Nálezová databáze ochrany přírody AOPK ČR (2025); Mertlík (2020 and 2021)"
  )

print(map)
