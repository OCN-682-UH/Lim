### MORE plotting with ggplot using data from the "palmerpenguins" package
### Created by: Wei Shen Lim
### Created on: 2026-09-10
##########################################################################

## Load Libraries
library(palmerpenguins)
library(tidyverse)
library(dplyr) #to subset data
library(ggbeeswarm) #for swarm plot
library(calecopal) #color palette of California
library(colorBlindness) #check color for colorblind-friendly
library(here)

## View data
view(penguins)

## Subset data
data <- penguins %>%
  filter(!is.na(sex)) #remove data with NA for sex

## Plot body mass by species and sex
my_plot <- ggplot(data,
       aes(x = species, 
           y = body_mass_g,
           color = species)) +
  geom_violin(fill = "white") + #violin plot
  geom_beeswarm() + #swarm plot
  facet_wrap(~ sex, ncol = 2) + #wrap by species, and make it two columns
  guides(color = "none") + #remove color legend
  labs(title = "Body mass of penguins in Palmer Archipelago, Antarctica",
       x = "Species",
       y = "Body Mass (g)",
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_manual(values = cal_palette("superbloom3")) + #set color scale using palette from "calecopal"
  theme_classic() + #set overall theme to classic
  theme(plot.title = element_text(size = 20), #change size of different text elements
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 15),
        panel.background = element_rect(fill = "lightblue")) #change the fill of the rectangle element of the panel background)
my_plot

## Check palette for colorblind-friendly
cvdPlot(my_plot)

## Save output of the plot
my_plot #ggsave saves the last active plot
ggsave(here("Week_03", "Output", "homework_ggplot.png"),
       width = 12, height = 10) # set size in inches
