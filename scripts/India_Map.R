#devtools::install_github("thomasp85/patchwork")
#devtools::install_github("HBGDki/growthstandards")

pacman::p_load(tidyverse, ggExtra, ggmap, ggthemes, patchwork,
               fs, rnaturalearth, sf, RColorBrewer, maps)

india_sf <- ne_states(country = "India", returnclass = "sf") 

data(world.cities)

india_cities <- world.cities %>%
  filter(country.etc == "India") %>%
  arrange(desc(pop)) %>%
  slice(1:10)

  

# get COUNTY data for a given state
# state_sf_names <- india_sf %>%
#   mutate(lon=map_dbl(geometry, ~st_transform(st_centroid(.x)[[1]])), # add centroid values for labels
#          lat=map_dbl(geometry, ~st_transform(st_centroid(.x)[[2]])))
#
# http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_disputed_areas.zip'

disp_reg <- read_sf(dsn = "artifacts/ne_50m_admin_0_breakaway_disputed_areas") %>%
  filter(FID %in% c(7:14, 20))



colors_use <- brewer.pal(9, "YlOrRd")
colors_use[1] <- "#FFFFFF"

(india_map <- ggplot() +
  geom_sf(data = disp_reg, fill = "white") + 
  geom_sf(data = india_sf, fill = "white") +
#  geom_point(data = india_cities, aes(x = long, y = lat)) +
#  geom_label(data = india_cities, aes(x = long, y = lat, label = name)) +
#  scale_fill_brewer(palette = "YlOrRd", na.value = "white") +
  scale_fill_manual(values = colors_use) +  
  theme_bw() +
  theme(legend.position = "bottom", axis.text = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "lightgrey"), panel.grid = element_blank(), 
        legend.text = element_text(size = 14), legend.title = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  labs(fill = "Number of\nsubjects", subtitle = " ") +
  coord_sf(datum = NA))
# , crs = sf::st_crs(24378))

# (india_bar <- province_counts %>%
#   mutate(province = str_replace_all(province, "OR", "OD")) %>%
#   ggplot(aes(x = reorder(province, n_subjects), y = n_subjects)) +
#   geom_col(aes(fill =  factor(round(n_subjects, -3))), color = "black", show.legend = FALSE) +
#   geom_text(aes(label = n_studies), vjust = -.5) +
#   theme_bw() +
# #  scale_fill_brewer(palette = "YlOrRd", na.value = "white") +  
#   scale_fill_manual(values = colors_use[-1]) +  
#   theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 14)) +
#   scale_y_continuous(trans = "sqrt", breaks = c(100, 500, 2500, 7000, 32500, 75000, 125000, 165000)) +
#   labs(x = "Region\n", y = "Number of subjects", title = "HBGDki subject counts within India", 
#        subtitle = "Study counts per region labeled\n\n\n"))
# 
# 
# ggsave(path("analysis", "rally_prep", "background_rally", "results", "india_map.png"),
#        plot = india_bar + india_map + plot_layout(ncol = 2, widths = c(2,3)), width = 12, height = 8)
