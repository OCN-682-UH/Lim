### Intro to Maps I
### Created by: Wei Shen Lim
### Created on: 2026-10-06
##########################################################################

## Load Libraries
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

## Load Population Data in California by county
popdata <- read_csv(here("Week_07", "Data", "CApopdata.csv"))

## Load Seastar Count Data at field sites in California
stars <- read_csv(here("Week_07", "Data", "stars.csv"))

## Use "maps" package to pull polygon boundaries for countries, states, and counties
world <- map_data("world") #polygon data for the whole world
head(world)

usa <- map_data("usa") #polygon data for USA
head(usa)

italy <- map_data("italy") #same thing with different countries
head(italy)

states <- map_data("state") #only USA states but not HI
head(states)

counties <- map_data("county") #only USA counties but not HI
head(counties)

## Map a map of the world
ggplot() +
  geom_polygon(data = world, #for creating map
               aes(x = long, y = lat,
                   group = group, #points in the same group get connected to form the regions
                   fill = region), #fill by region (from the data frame)
               color = "black") + #add color to lines and fill
  guides(fill = "none") + #remove legend for the "fill" aesthetic
  theme_minimal() + #changing theme
  theme(panel.background = 
          element_rect(fill = "lightblue")) + # make ocean blue
  coord_map(projection = "mercator",
            xlim = c(-180,180)) #set the map projection (check ?mapproj::mapproject for other options)

## Make a map of just California
CA_data <- counties |> 
  filter(region == "california")  #load county data of USA and filter by CA

ggplot() +
  geom_polygon(data = CA_data,
               aes(x = long, y = lat,
                   group = group,
                   fill = subregion), #fill by subregion
               color = "black") +
  guides(fill = "none") #remove legend for "fill" aesthetic
  coord_map() + #default is mercator projection
  theme_minimal()

## Adding multiple layers of the data
#Plot the population of every county in California
head(counties)
head(popdata)

#Join both datasets
CApop_county <- popdata |> 
  select("subregion" = County, Population) |> #rename 'County' column to match
  inner_join(counties) |> #keep only rows that exist in both dataframes
  filter(region == "california") #some county names repeat in other states so filter by California
head(CApop_county)

#Map CA population by county
ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long, y = lat,
                   group = group,
                   fill = Population), #fill by population data
               color = "black") +
  coord_map() +
  theme_void() +
  scale_fill_gradient(transform = "log10") #log scale for easier interpretation

## Add a layer of points
head(stars)

ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long, y = lat,
                   group = group,
                   fill = Population), #fill by population data
               color = "black") +
  geom_point(data = stars, #overlay seastar sites
             aes(x = long,
                 y = lat,
                 size = star_no)) + #size points by seastar count
  coord_map() +
  theme_void() +
  scale_fill_gradient(transform = "log10") + #log scale for easier
  labs(size = "# stars/m²") #rename legend label

## Save map output
ggsave(here("Week_07", "Output", "CApop.pdf")) #pdf for fun, no reason

## The modern approach, "sf" and "rnaturalearth" packages
## Load Libraries
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world_sf <- ne_countries(scale = "medium",
                         returnclass = "sf") #load world data as an "sf" object

ggplot(world_sf) +
  geom_sf(fill = "lightgray",
          color = "white") +
  coord_sf(crs = "+proj=robin") + #Robinson projection
  theme_void()

## Fun package! Plot with dogs in ggplot2
library(ggdogs)
ggplot(mtcars) +
  geom_dog(aes(mpg, wt), dog = "pug", size = 5)