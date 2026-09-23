### Advanced plotting
### Created by: Wei Shen Lim
### Created on: 2026-09-22
##########################################################################

## Load Libraries
library(patchwork) #for bringing plots together
library(ggrepel) #for repelling labels
library(gganimate) #smooth animations
library(gifski) #for saving gifs
library(plotly) #for interactive animations
library(magick) #for images
library(palmerpenguins)
library(here)
library(sourrr) #Sourdough recipes~

############################### Part 1: patchwork############################
# Create two simple plots
p1 <- penguins |> 
  ggplot(aes(x = body_mass_g,
             y = bill_length_mm,
             color = species)) +
  geom_point()
p1

p2 <- penguins |> 
  ggplot(aes(x = sex,
             y = body_mass_g,
             color = species)) +
  geom_jitter(width = 0.2)
p2

# Combine plots with +
p1 + p2 +
  plot_layout(guides = 'collect') + #collect legends, basically combine them
  plot_annotation(tag_levels = 'A') #add plot annotations

# Stack plots vertically with /
p1 / p2 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')

########################## Part 2: ggrepel ##################################
head(mtcars) #use built-in dataset with ggplot

# Simple plot without label repelling
ggplot(mtcars, aes(x = wt,
                   y = mpg,
                   label = rownames(mtcars))) + #rownames because does not have a column title
  geom_text() + #add those labels on top
  geom_point(color = 'red')

# Now make plot with ggrepel
ggplot(mtcars, aes(x = wt,
                   y = mpg,
                   label = rownames(mtcars))) + 
  geom_text_repel() + #repel labels
  geom_point(color = 'red')

# Add boxes to those labels
ggplot(mtcars, aes(x = wt,
                   y = mpg,
                   label = rownames(mtcars))) + 
  geom_label_repel() + #add boxes to labels
  geom_point(color = 'red')

########################### Part 3: gganimate ###############################
# Static penguin plot
penguins |> 
  ggplot(aes(x = body_mass_g,
             y = bill_depth_mm,
             color = species)) +
  geom_point()

# Add transition, animate between year
p <- penguins |> 
  ggplot(aes(x = body_mass_g,
             y = bill_depth_mm,
             color = species)) +
  geom_point() +
  transition_states(
    year, #the thing you want to transition
    transition_length = 2, #2 secs to move from one to the next
    state_length = 1) + #how long it stays in that transition (1 sec)
  labs(title = 'Year: {closest_state}') #add dynamic title, {closest_state} updates with each frame

# Save animation as a gif
anim_save(here("Week_05", "Output", "penguin_animation.gif"), animation = p)

############################ Part 4: plotly #################################
# Create an interactive scatter plot
penguins |> 
  plot_ly(x = ~body_mass_g,
          y = ~bill_depth_mm,
          color = ~species,
          type = "scatter",
          mode = "markers") |> #hover marker over points to get data
  layout(title = "Penguin Body Mass vs Bill Depth",
         xaxis = list(title = "Body Mass (g)"),
         yaxis = list(title = "Bill Depth (mm"))

# Animate by species with frame
penguins |> 
  plot_ly(x = ~body_mass_g,
          y = ~bill_depth_mm,
          frame = ~species, #animate species by frame
          color = ~species,
          type = "scatter",
          mode = "markers",
          marker = list(size = 8)) |> #change marker size
  layout(title = "Penguin Characteristics",
         xaxis = list(title = "Body Mass (g)"),
         yaxis = list(title = "Bill Depth (mm)"))

############################# Part 5: magick ################################
# Read an image with image_read()
penguin <- image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png")
penguin

# Save a plot as an image
penguinplot <- penguins |> 
  ggplot(aes(x = body_mass_g,
             y = bill_depth_mm,
             color = species)) +
  geom_point()
ggsave(here("Week_05", "Output", "penguinplot.png"))
penguinplot

# Composite images with image_composite()
penplot <- image_read(here("Week_05", "Output", "penguinplot.png"))
out <- image_composite(image = penplot, #background image
                       composite_image = penguin, #image that goes on top
                       offset = "+70+30") #puts image on the top 70% and left 30%
out

# Combine GIFs with images
pengif <- image_read("https://media3.giphy.com/media/H4uE6w9G1uK4M/giphy.gif")
outgif <- image_composite(penplot, pengif, gravity = "center") #gravity pulls where you want your image to go
animation <- image_animate(outgif, fps= 10, optimize = TRUE) #save this as an animation
animation

## Fun package! Sourdough recipes~
build_recipe(final_weight = 900, hydration = 0.75)