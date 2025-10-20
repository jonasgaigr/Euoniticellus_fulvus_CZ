#----------------------------------------------------------#
# Plot occurrence -----
#----------------------------------------------------------#
map <- ggplot() +
  # raster background
  tidyterra::geom_spatraster(data = hill) +
  scale_fill_gradientn(
    colors = c(
      scales::alpha("white", 0),        # flat white
      scales::alpha("grey90", 0.1),
      scales::alpha("grey80", 0.2),
      scales::alpha("grey70", 0.4),
      scales::alpha("grey60", 0.6)      # more visible shadows
    ),
    na.value = "white",
    guide = "none"
  ) +

  # Allow a new fill scale for points
  ggnewscale::new_scale_fill() +
  
  # Rivers
  geom_sf(data = rivers, color = "steelblue", size = 0.25) +
  
  # Czechia border
  geom_sf(data = czechia_border, fill = NA, color = "black", size = 0.5) +
  
  geom_sf(data = sitmap, fill = NA) +
  
  # Historical polygons (cross-hatch effect)
  geom_sf(data = map_data_hist, fill = "grey30", color = NA, alpha = 0.5) +
  
  # Recent points colored by period
  #geom_sf(data = map_data_rec, aes(fill = min_year), color = "black", size = 0.15) +
  geom_sf(
    data = sf::st_buffer(map_rings_rec, dist = 4300),
    aes(fill = min_year),
    color = NA,       # subtle outline for contrast
    size = 3              # outline thickness
  ) +
  scale_fill_gradient2(
    name = "Rok prvního nálezu\npo roce 1975\n",
    low = "#4575B4", mid = "#FFFFBF", high = "#D73027",
    midpoint = 2015, na.value = "grey90"
  ) +
  scale_fill_viridis_c(
    name = "Rok prvního nálezu\npo roce 1975\n",
    #option = "magma",
    direction = -1,    # reverse so recent years are lighter or more vivid
    begin = 0.1,
    end = 0.9,
    na.value = "grey90"
  ) +
  # Coordinate grid
  coord_sf(crs = st_crs(4326), 
           xlim = c(st_bbox(sitmap)["xmin"], st_bbox(sitmap)["xmax"]),
           ylim = c(st_bbox(sitmap)["ymin"], st_bbox(sitmap)["ymax"]),
           expand = FALSE) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "grey20", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey80", size = 0.4),
    axis.text = element_text(size = 8)
  ) +
  labs(
    title = expression(paste("Rozšíření ", italic("Euoniticellus fulvus"), " (Goeze, 1777)")),
    subtitle = "Rozšíření před rokem 1975 šedě, pozdější nálezy barevně",
    caption = "Data: Nálezová databáze ochrany přírody AOPK ČR (2025); Mertlík (2020 and 2021)"
  )

print(map)

ggsave(
  filename = "Outputs/Plots/euoniticellus_map.png",   # output file name
  plot = map,                     # the ggplot object
  width = 8,                            # width in inches
  height = 6,                           # height in inches
  dpi = 300                             # resolution
)

#----------------------------------------------------------#
# Histogram -----
#----------------------------------------------------------#
occ_hist <- ggplot(data_agg, aes(x = year)) +
  geom_histogram(
    binwidth = 1,
    fill = "palegreen4",
    alpha = 0.6,
    color = "white"
  ) +
  
  # Custom theme (you can replace with theme_niwot() if available)
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  
  # Y-axis limits and expansion
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  # Vertical line at mean year
  geom_vline(
    xintercept = mean(data_agg$year, na.rm = TRUE),
    linetype = "dotted",
    colour = "palegreen4",
    size = 1
  ) +
  
  # Annotation text and arrow
  annotate(
    "text",
    x = mean(data_agg$year, na.rm = TRUE) + 3,
    y = max(table(data_agg$year)) * 0.7,
    label = paste0(
      "The mean observation year\nwas ",
      round(mean(data_agg$year, na.rm = TRUE))
    ),
    hjust = 0,
    color = "grey20"
  ) +
  geom_curve(
    aes(
      x = mean(data_agg$year, na.rm = TRUE) + 2,
      y = max(table(data_agg$year)) * 0.85,
      xend = mean(data_agg$year, na.rm = TRUE) + 0.3,
      yend = max(table(data_agg$year)) * 0.8
    ),
    arrow = arrow(length = unit(0.07, "inch")),
    size = 0.7,
    color = "grey30",
    curvature = 0.3
  ) +
  
  # Axis labels
  labs(
    x = "\nObservation year",
    y = "Number of occurrences\n"
  )

occ_hist

