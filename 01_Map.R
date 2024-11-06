

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


#qc_subset_sf$RiverName
# Create the map
site_map <- ggplot() +
  # Add lakes, rivers and stream lines 
  geom_sf(data = world, fill = "white", color = "grey80") +
  geom_sf(data = streams_QC, color = "steelblue1", alpha=0.2, size = 0.05) +
  geom_sf(data = watercourse_QC, color = "steelblue1", size = 0.5) +
  geom_sf(data = waterbody_QC, fill = "steelblue", color="steelblue3", size = 0.5) +
  
  
  # Add latitude longitude lines
  geom_hline(yintercept = seq(48, 54, by = 3), color = "gray50", linetype = "dashed", size = 0.3) +
    geom_vline(xintercept = seq(-80, -54, by = 4), color = "gray50", linetype = "dashed", size = 0.3) +
  
  # Add labels for latitude longitude lines
  geom_text(aes(x = -82, y = seq(48, 54, by = 3), label = paste0(seq(48, 54, by = 3), "°N")), 
            hjust = 0, vjust = -0.5, size = 3, color = "gray50") +
    geom_text(aes(x = seq(-80, -54, by = 4), y = 47, label = paste0(abs(seq(-80, -54, by = 4)), "°W")), 
            hjust = 0.5, vjust = 1, size = 3, color = "gray50") +
  
  
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



#ggplotly(site_map, tooltip = c("fill","label", "text"))











# Codes for making the sub_maps

#LRmap= quebec+ #La Romaine ZOOMED IN
#  coord_sf(xlim = c(-65, -62), ylim = c(50.2, 52.5))+
#  scale_x_continuous(breaks=1)+
#  scale_y_continuous(breaks=1)+
#  labs(x="", y="")

#LRmap

#View(filter(qc_subset, Region=="HM"))

#HMmap= quebec+ #Haute Mauricie ZOOMED IN
#  coord_sf(xlim = c(-74.5, -72.5), ylim = c(48.5, 47.5))+
  #scale_size((50)) +
#  scale_x_continuous(breaks=1)+
#  scale_y_continuous(breaks=1)+
#  labs(x="", y="")

#HMmap

#NSmap= quebec+ #Niskamoon ZOOMED IN
#  coord_sf(xlim = c(-80, -76), ylim = c(50, 55))+
#  scale_x_continuous(breaks=1)+
#  scale_y_continuous(breaks=1)+
#  labs(x="", y="")
#NSmap

#quartz(width=12,height=5,pointsize=12)
#ggarrange(NSmap, HMmap, LRmap, nrow=1, labels=c("B)", "C)", "D)") )
#quartz.save("/Users/audreycampeau/Documents/DATA/JF/R Codes/JF/Submaps.png", type="png", dpi=600)
