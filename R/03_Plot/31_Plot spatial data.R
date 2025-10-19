#--------------------------------------------------#
# Plot spatial data
#--------------------------------------------------#
p2 <- ggplot() +
  geom_sf(data = czechia_border,
          fill = NA) +
  geom_sf(data = sitmap,
          fill = NA) +
  # use cross-hatch pattern for historical polygons
  geom_sf(data = map_data_hist, fill = "grey70", color = NA, alpha = 0.5) +
  # recent continuous fill
  geom_sf(data = map_data_rec, 
          aes(fill = max_year),
          color = "black", size = 0.15) +
  scale_fill_gradient2(
    name = "Rok prvního nálezu po roce 1975",
    low = "#4575B4",       # deep blue – older
    mid = "#FFFFBF",       # neutral yellow
    high = "#D73027",      # vivid red – recent
    midpoint = 2017,
    na.value = "grey90"
  ) +
  labs(
    title = "Rozšíření Euoniticellus fulvus (Goeze, 1777)",
    subtitle = "Rozšíření před rokem 1975 šrafovaně, pozdější nálezy barevně",
    caption = "Data: Nálezová databáze ochrany přírody AOPK ČR (2025); Mertlík (2020 and 2021)"
  ) +
  theme_minimal()

print(p2)