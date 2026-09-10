### Plotting with ggplot using data from the "palmerpenguins" package
### Created by: Wei Shen Lim
### Created on: 2026-09-09
###########################################

## Load Libraries
library(palmerpenguins)
library(tidyverse)

## View data
glimpse(penguins)

## Exploring mapping options of ggplot
ggplot(data = penguins, #set data frame
       mapping = aes(x = bill_depth_mm, #arguments in aes = mapping
                     y = bill_length_mm,
                     color = species,
                     shape = island,
                     size = body_mass_g,
                     alpha = flipper_length_mm)) + #alpha = transparency
  geom_point() + #arguments in geoms = setting
  labs(title = "Bill depth and length",
       subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins",
       x = "Bill depth (mm)",
       y = "Bill length (mm)",
       color = "Species",
       shape = "Island", 
       size = "Body Mass (g)", 
       alpha = "Flipper Length (mm)", 
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d() #color scale for color blindness

## Faceting (smaller plots that display different subsets of the data)
#facet_grid #must be a grid
ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm)) +
  geom_point() +
  facet_grid(species~sex) #makes multiple plots group by species (rows) and sex (columns)

#facet_wrap (can manipulate rows and columns)
ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm)) +
  geom_point() +
  facet_wrap(~ species, ncol = 2) #wrap by species, and make it two columns

## Combine facet with others
ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) +
  geom_point() +
  scale_color_viridis_d() +
  facet_grid(species~sex) +
  guides(color = "none") #remove redundant legend
