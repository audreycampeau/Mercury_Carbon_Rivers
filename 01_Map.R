

library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(ggspatial)
install.packages("rnaturalearthdata")
library(ggmap)
library(plotly)
library(stringr)

# Read the feature classes
gdb <- "/Users/audreycampeau/Documents/DATA/JF-Mercury/R/GIS data/canvec_5M_CA_Hydro_fgdb - Copy/canvec_5M_CA_Hydro.gdb"

waterbody <- st_read(dsn = gdb, layer = "waterbody_2")
watercourse <- st_read(dsn = gdb, layer = "watercourse_1")

# Filter for Quebec
waterbody_QC <- waterbody %>% 
  filter(political_division == "102") %>% 
  st_transform(4326)  # Transform to WGS84

watercourse_QC <- watercourse %>% 
  filter(political_division == "102") %>% 
  st_transform(4326)  # Transform to WGS84




#Read the shapefile from HydroRiver database instead of gouv. data
gdb2 <- "/Users/audreycampeau/Documents/DATA/JF-Mercury/R/GIS data/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp"
streams <- st_read(gdb2)
streams<- fortify(streams)
streams %>% filter(str_extract(HYRIV_ID, "[0-9]+") %in% 7)

#Filter for Québec
streams_QC <- streams %>%
  st_crop(xmin = -84, xmax = -50, ymin = 45, ymax = 58) %>%
  st_transform(4326)  # Ensure it's in WGS84



# Import world map
world <- ne_countries(scale = "medium", returnclass = "sf")


# Convert qc_subset to sf object
qc_subset_sf <- qc_subset %>%
  filter(!is.na(GPS_W) & !is.na(GPS_N)) %>% # Handle missing coordinates in qc_subset
  st_as_sf(coords = c("GPS_W", "GPS_N"), crs = 4326)

# Print summary to check conversion
cat("Original number of points:", nrow(qc_subset), "\n")
cat("Number of points with valid coordinates:", nrow(qc_subset_sf), "\n")






# Create the map
site_map <- ggplot() +
  # Add lakes, rivers and stream lines 
  geom_sf(data = world, fill = "white", color = "grey95") +
  geom_sf(data = streams_QC, color = "steelblue1", alpha=0.2, size = 0.05) +
  geom_sf(data = watercourse_QC, color = "steelblue1", size = 0.5) +
  geom_sf(data = waterbody_QC, fill = "steelblue", color="steelblue3", size = 0.5) +
  
  
  # Add latitude longitude lines
  geom_hline(yintercept = seq(48, 54, by = 3), color = "gray40", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = seq(-80, -54, by = 4), color = "gray40", linetype = "dashed", size = 0.3) +
  
  # Add labels for latitude longitude lines
  geom_text(aes(x = -82, y = seq(48, 54, by = 3), label = paste0(seq(48, 54, by = 3), "°N")), 
            hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
  geom_text(aes(x = seq(-64, -54, by = 4), y = 47, label = paste0(abs(seq(-64, -54, by = 4)), "°W")), 
            hjust = 0.5, vjust = 1, size = 3, color = "gray40") +
  
  # Add north arrow
  annotation_north_arrow(
    location = "tr",  # top right position
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  
  # Add projection text
  annotate("text", x = -56, y = 47.5, 
           label = "Projection: WGS 84\nEPSG:4326", 
           size = 3, color = "gray30", hjust = 0) +
  
  
  geom_sf(data = qc_subset_sf, aes(fill = Region, label=RiverName, shape=Region), size = 3) +
  coord_sf(xlim = c(-82, -52), ylim = c(47, 55)) +
  scale_shape_manual(values=c(21,22,24))+
  scale_fill_manual(values = colorramp) +
  annotation_scale(location = "bl", width_hint = 0.5, text_cex = 1) +
  labs(y = "Latitude", x = "Longitude") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = "azure3"),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
  )

# Display the map
print(site_map)











