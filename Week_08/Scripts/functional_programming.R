### Functional Programming
### Created by: Wei Shen Lim
### Created on: 2026-10-13
##########################################################################

## Load libraries
library(tidyverse)
library(palmerpenguins)
library(PNWColors) #Pacific Northwest Color Palette
library(BRRR) #Sounds with your favorite rappers

#################### Rescaling data to lie between 0 and 1 ####################
df <- tibble(
  a = rnorm(10), #rnorm = a set of 10 random numbers
  b = rnorm(10), #set seed if you want it to be the same every time
  c = rnorm(10),
  d = rnorm(10)
)

head(df)

#Without function: Manual rescaling
df <- df |> 
  mutate(a = (a - min(a, na.rm = TRUE)) / (max(a, na.rm = TRUE) - min(a, na.rm = TRUE)))

head(df)

#With function
rescale01 <- function(x) {
  value <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return(value)
}

df |> 
  mutate(
    a = rescale01(a),
    b = rescale01(b),
    c = rescale01(c),
    d = rescale01(d)
  )

#Even better: use mutate(across(everything()))
df |> 
  mutate(across(everything(), rescale01))

##################### Example: Fahrenheit to Celsius ##########################
#Step 1: The conversion formula
temp_C <- (temp_F - 32) * 5 / 9

#Step 2: Create the function structure
f_to_celsius <- function() {
  
}

#Step 3: Add the formula
f_to_celsius <- function() {
  temp_C <- (temp_F - 32) * 5 / 9
}

#Step 4: Add the argument
f_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
}

#Step 5: Add the return statement
f_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

#Step 6: Test it
f_to_celsius(32)
f_to_celsius(212)

############################ Now do Celsius to Kelvin #########################
celsius_to_kelvin <- function(temp_C) {
  temp_K <- (temp_C + 273.15)
  return(temp_K)
}

celsius_to_kelvin(1)

############################# Making Plot Functions ###########################
#First create a resusable plot function
pal <- pnw_palette("Lake", 3, type = "discrete")

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual("Island", values = pal) +
  theme_bw()

#Name and set up the function
myplot <- function() { #assign function to a name
  pal <- pnw_palette("Lake", 3, type = "discrete")
  
  ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_color_manual("Island", values = pal) +
    theme_bw()
}

#Add flexible arguments
myplot <- function(data, x, y) { #set function arguments
  pal <- pnw_palette("Lake", 3, type = "discrete")
  
  ggplot(data, aes(x = {{x}}, y = {{y}}, color = island)) + #need curly-curly {{}} to pass column names as arguments
    geom_point() +
    geom_smooth(method = "lm") +
    scale_color_manual("Island", values = pal) +
    theme_bw()
}

#Test it
myplot(data = penguins, x = body_mass_g, y = bill_length_mm)
myplot(data = penguins, x = body_mass_g, y = flipper_length_mm)

#Add default arguments
myplot <- function(data = penguins, x, y) { #set fix dataset
  pal <- pnw_palette("Lake", 3, type = "discrete")
  
  ggplot(data, aes(x = {{x}}, y = {{y}}, color = island)) + #now you need to have the island column in your dataset, to make it flexible, put color = {{color}}, and add it to your functions' arguments (data, x, y, color)
    geom_point() +
    geom_smooth(method = "lm") +
    scale_color_manual("Island", values = pal) +
    theme_bw()
}

#Test it with just column argument
myplot(x = body_mass_g, y = flipper_length_mm) +
  labs(x = "Body mass (g)", y = "Flipper length (mm)") #adding layers like usual

############################ if-else statement ################################
a <- 4
b <- 5

if (a > b) {
  f <- 20
} else {
  f <- 10
}

f

#Making plot function more flexible
myplot <- function(data = penguins, x, y, lines = TRUE) { #dictate what the argument is (here setting default as TRUE), anything else has to be in quotes
  pal <- pnw_palette("Lake", 3, type = "discrete")
  
  if (lines == TRUE) {
    ggplot(data, aes(x = {{x}}, y = {{y}}, color = island)) +
      geom_point() +
      geom_smooth(method = "lm") +
      scale_color_manual("Island", values = pal) +
      theme_bw()
  } else {
    ggplot(data, aes(x = {{x}}, y = {{y}}, color = island)) +
      geom_point() +
      scale_color_manual("Island", values = pal) +
      theme_bw()
  }
}

#Test with lines (default)
myplot(x = body_mass_g, y = flipper_length_mm)

#Test without lines
myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE)

## Fun package! Sounds with your favorite rappers!
skrrrahh("snoop")
skrrrahh("biggie")