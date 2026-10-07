### Intro to Maps II
### Created by: Wei Shen Lim
### Created on: 2026-10-06
##########################################################################

## Load Libraries
library(tidyverse)
library(here)
library(sf)
library(maptiles) #download tile basemaps (satellite, street, terrain)
library(tidyterra) #plot terra rasters in ggplot2
library(ggspatial) #scale bars and north arrows
library(leaflet) #interactive HTML maps
library(tidygeocoder) #free geocoding (OpenStreetMap / Census)

## Load Data
ChemData <- read_csv(here("Week_07", "Data", "chemicaldata_maunalua.csv"))

## Getting Tile Basemaps with "maptiles"
#1)Define a bounding box around Wailupe, Oahu
wailupe_bbox <- st_as_sfc( #convert to a "sf" object so "maptiles" can read it
  st_bbox(c(xmin = -157.772, ymin = 21.267, #westmost & southermost longitude
            xmax = -157.752, ymax = 21.281), #eastmost & northernmost latitde
          crs = 4326) #this means WGS84 - the lat/lon system your GPS uses
)

#2)Download the tiles
tiles_satellite <- get_tiles(
  wailupe_bbox, #the area to download
  provider = "Esri.WorldImagery", #which tile source to use
  zoom = 16, #level of detail (3 = continent, 20 = building)
  crop = TRUE, #clip to our exact box (vs. slightly larger)
  cachedir = here("Week_07", "Data", "tiles_cache") #save tiles to disk
)
class(tiles_satellite) #this is returning a "SpatRaster" file in the "terra" package

#3)Plot with geom_spatraster_rgb()
ggplot() +
  geom_spatraster_rgb(data = tiles_satellite) + #use this instead of geom_raster() because geom_raster() expects a dataframe, not a terra object
  coord_sf(crs = 4326) + #tells ggplot to treat your axes as geographic coordinates in WGS84, always include when plotting spatial data
  theme_minimal()

## Derive the bounding box from your data
#1)Convert dataframe to an sf object
ChemData_sf <- st_as_sf(ChemData,
                        coords = c("Long", "Lat"), #tells sf which columns hold longitude and latitude
                        crs = 4326)

#2)Get bounding box and add a small buffer
bbox_chem <- st_bbox(ChemData_sf) + c(-0.004, -0.004, 0.004, 0.004) #pads all four sides by ~0.4km so points aren't at the edge

#3)Download tiles for that extent
Map_satellite <- get_tiles(
  st_as_sfc(bbox_chem),
  provider = "Esri.WorldImagery",
  zoom = 15,
  crop = TRUE,
  cachedir = here("Week_07", "Data", "tiles_cache")
)

## Overlay Point Data & Add Scale Bar + North Arrow ("ggspatial" package)
ggplot() +
  geom_spatraster_rgb(data = Map_satellite) +
  geom_point(data = ChemData, #overlay point data
             aes(x = Long, y = Lat,
                 color = Salinity),
             size = 4) +
  scale_color_viridis_c(name = "Salinity (ppt)") + #rename color scale legend
  annotation_scale(location = "bl", #scale bar, bl = bottom left
                   bar_cols = c("yellow", "white")) +
  annotation_north_arrow(location = "tl") + #north arrow, tl = top left
  coord_sf(crs = 4326) + #tells ggplot to treat your axes as geographic coordinates in WGS84, always include when plotting spatial data
  labs(title = "Maunalua Bay - Water Chemistry",
       caption = "Basemap: ESRI World Imagery") +
  theme_minimal()

#Save Map Output
ggsave(here("Week_07", "Output", "maunalua_map.pdf"), width = 8, height = 6)

## Interactive Maps with "leaflet"
#similar in concept to ggplot but with |>  instead of +
leaflet() |> #create the map widget
  addProviderTiles(providers$Esri.WorldImagery) |> #add the basemap tile layer
  setView(lng = -157.762, lat = 21.274, zoom = 14) #centers the map on a specific location (long, lat)

#Add a Color Palette
#leaflet needs a color function that converts numeric values to hex colors
pal <- colorNumeric(palette = "viridis", #colorNumeric returns a function, any RColorBrewer or viridis palette name
                    domain = ChemData$Salinity) #the full range of values to map

#Add Data Points with Color after Creating Color Function
leaflet(ChemData) |> #pass dataframe here
  addProviderTiles(providers$Esri.WorldImagery) |> #add the basemap tile layer
  addCircleMarkers(lng = ~Long, #adding the points
                   lat = ~Lat,
                   color = ~pal(Salinity), #apply color function to Salinity
                   radius = 6,
                   fillOpacity = 0.9,
                   label = ~Site, #similar to popup but appears when you hover over a point
                   popup = ~paste("Salinity:", Salinity, "ppt", #popup message when you click on a point
                                  "<br>Site:", Site)) |>  #<br> = line break
  addLegend(pal = pal, #the same color function
            values = ~Salinity, #which column drives the legend
            title = "Salinity (ppt)",
            position = "bottomright")

## Geocoding with "tidygeocoder"
#converts a place name or address into lat and long coordinates
#Convert a Place Name to Coordinates
geo("University of Hawaii at Manoa", method = "osm") #osm best for landmarks and place names worldwide
geo("Pearl Harbor, Oahu, Hawaii", method = "osm") #if returned NA, try more specific names (in this case, Pearl Harbor National Memorial)

#Geocode a Data Frame
oahu_sites <- tribble(
  ~place,
  "University of Hawaii at Manoa",
  "Honolulu International Airport",
  "Pearl Harbor National Memorial, Hawaii",
  "Diamond Head State Monument, Hawaii"
)

oahu_geocoded <- oahu_sites |>
  geocode(place, method = "osm") # geocode the 'place' column
oahu_geocoded

#Map Geocoded Locations
leaflet(oahu_geocoded) |> 
  addProviderTiles(providers$Esri.WorldImagery) |> 
  addMarkers(lng = ~long, 
             lat = ~lat, #tidygeocoder always returns 'lat' and 'long'
             label = ~place) #hover to see place name

## Fun Package! Plot with different emojis
library(emojifont)
search_emoji("smile")
ggplot() +
  geom_emoji("smile_cat",
             x = 1:5, y = 1:5,
             size = 10)
