### Data wrangling with dpylr using data from the "palmerpenguins" package
### Created by: Wei Shen Lim
### Created on: 2026-09-19
##########################################################################

## Load Libraries
library(palmerpenguins)
library(tidyverse)
library(ggbeeswarm) #for swarm plot
library(calecopal) #color palette of California
library(here)

## View data
view(penguins)

## Calculate mean and variance of body mass by species, island and sex without any NAs
penguins_1 <- penguins |> 
  drop_na(sex) |> #remove data with NA for sex
  group_by(species, island, sex) |> #group by species, island and sex
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE), #calculate mean body mass without any NAs
            var_body_mass = var(body_mass_g, na.rm = TRUE)) #calculate variance of body mass without any NAs

## View output
view(penguins_1)

## Filter out males, then calculate log body mass, then select only columns for species, island, sex, and log body mass, then make plot
penguins_2 <- penguins |> 
  filter(sex != "male") |> #filter out males
  mutate(log_body_mass = log(body_mass_g)) |> #create new column for calculated log body mass
  select(species, island, sex, log_body_mass) |> #select species, island, sex, and log body mass
  ggplot(aes(x = species, #similar plot as HW3 for comparison
             y = log_body_mass,
             color = species)) +
  geom_violin(fill = "white") + #violin plot
  geom_beeswarm() + #swarm plot
  geom_boxplot(fill = NA) + #box plot with no fill to avoid blocking violin & swarm plots
  guides(color = "none") + #remove color legend
  labs(title = "Body mass of female penguins in Palmer Archipelago, Antarctica",
       x = "Species",
       y = expression(Log[10]~("Body Mass [g]")), #expression to label log 10
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_manual(values = cal_palette("superbloom3")) + #set color scale using palette from "calecopal"
  theme_classic() + #set overall theme to classic
  theme(plot.title = element_text(size = 20), #change size of different text elements
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 5),
        panel.background = element_rect(fill = "lightblue")) #change the fill of the rectangle element of the panel background

## View plot output
penguins_2

## Save plot output
ggsave(here("Week_04", "Output", "homework_dplyr.png"), penguins_2, #clarify which plot to save
       width = 12, height = 10) #set size in inches
