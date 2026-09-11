### More plotting with ggplot using data from the "palmerpenguins" package
### Created by: Wei Shen Lim
### Created on: 2026-09-09
##########################################################################

## Load Libraries
library(palmerpenguins)
library(tidyverse)
library(here)
library(beyonce)

## View data
glimpse(penguins)

## Adding multiple geoms
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point() +
  geom_smooth() + #add a smooth best fit line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)")

#make it a linear model
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) + #given the data, probably want to look at them by species
  geom_point() +
  geom_smooth(method = "lm") + #linear
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)") +
  scale_color_viridis_d()

## Changing scales
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(14, 17, 21),
                     labels = c("low", "medium", "high")) #set x-axis breaks and labels

#manually change color scale
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)") +
  scale_color_viridis_d() +
  scale_color_manual(values = c("orange", "purple", "green")) #set color scale manually

#or use a package
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(2)) #installed package directly from Github using "pak"

## Coordinates: flipping the axes
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(2)) +
  coord_flip() #flip x and y axes

#fix the axes
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(2)) +
  coord_fixed() #fix axes

#transform x and y-axis (log10), using a different data set in ggplot
ggplot(diamonds, #new data set from ggplot
       aes(carat, price)) +
  geom_point() +
  coord_trans(x = "log10", y = "log10") #easier for reader than changing the values displayed on the axes

#make coordinates polar
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(2)) +
  coord_polar("x") #make x-axis polar

## Themes (the non-data elements, setting)
plot1 <- ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)") +
  scale_color_manual(values = beyonce_palette(2)) +
  theme_classic() + #example of using complete themes
  theme(axis.title = element_text(size = 20,
                                  color = "red"), #change the size and color of the text element of the axis title
        panel.background = element_rect(fill = "linen")) #change the fill of the rectangle element of the panel background

## Save the output of the plot
ggsave(here("Week_03", "Output", "penguin.png"),
width = 7, height = 5) # set size in inches
