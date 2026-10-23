### Working with Factors
### Created by: Wei Shen Lim
### Created on: 2026-10-21
##########################################################################

## Load libraries
library(tidyverse)
library(here)
library(gm) #Generate music notation and audio!

## Load Data
#Option 1: via tidytuesdayR
income_mean <- tidytuesdayR::tt_load(2021, week = 7)$income_mean

# Option 2: read directly from GitHub
income_mean <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv")

############################### Factor ########################################
fruits <- factor(c("Apple", "Grape", "Banana")) #convert vector to factor
fruits #notice the order of the output is in alphabetical order (default)

########################## Factor Booby-Trap ##################################
test <- c("A", "1", "2") #column of numbers with type, this is read as characters by R
as.numeric(test) #if convert to numeric, the real text becomes an NA

test <- factor(test) #if convert to factor first
as.numeric(test) #it returns the underlying integer codes, not the original value, so now the "A" becomes "3" instead

#therefore always use read_csv() [Strings stay as characters] rather than read.csv() [Strings become factors automatically]

######################### the 'forcats' package ##############################
## Meet the starwars Data
glimpse(starwars)

#How many of each speices?
starwars |> 
  filter(!is.na(species)) |> #remove NAs
  count(species, sort = TRUE) #sorts output table in descending order

#fct_lump() - Lump rare levels together (everything else into "Other")
star_counts <- starwars |> 
  filter(!is.na(species)) |> 
  mutate(species = fct_lump(species, n = 3)) |> #keeps 3 most common levels
  count(species) #without sort, output is alphabetical order
star_counts

##################@####### Reordering factors #################################
#Basic bar plot
star_counts |> 
  ggplot(aes(x = species, y = n)) +
  geom_col()

#fct_reorder() - Sort by another variable
star_counts |> 
  ggplot(aes(x = fct_reorder(species, n), y = n)) + #reorder species by n
  geom_col()

#fct_reorder() - Descending order
star_counts |> 
  ggplot(aes(x = fct_reorder(species, n, .desc = TRUE), y = n)) +
  geom_col() +
  labs(x = "Species")

########################## Reorder line plot legends ##########################
## Meet income_mean Data
glimpse(income_mean)

#Summarize by Year and Quintile
total_income <- income_mean |> 
  group_by(year, income_quintile) |> 
  summarise(income_dollars_sum = sum(income_dollars), .groups = "drop") |> #drop groups that don't exist
  mutate(income_quintile = factor(income_quintile)) #mutate this column into a factor so we can reorder it

#Basic line plot - Legend in wrong order
total_income |> 
  ggplot(aes(x = year, y = income_dollars_sum,
             color = income_quintile)) + #legend does not match line order, in alphabetical order by default
  geom_line()

#fct_reorder2() - Reorder by two variables
total_income |> 
  ggplot(aes(x = year, y = income_dollars_sum,
             color = fct_reorder2(income_quintile, year, income_dollars_sum))) +
  geom_line() +
  labs(color = "income quintile")
#reorders income_quintile by the values of income_dollars_sum at the largest values of year (if you don't order by year, the order would be by average overall income, or simply the order they appear in the dataset)

######################## Setting levels manually ##############################
## factor() with levels = ...
#Use levels to specify your order
x1 <- factor(c("Jan", "Mar", "Apr", "Dec"),
             levels = c("Jan", "Mar", "Apr", "Dec")) #use levels= to specify
x1

############################## Subsetting factors #############################
#Filter to common species
starwars_clean <- starwars |> 
  filter(!is.na(species)) |>           #remove NAs
  count(species, sort = TRUE) |>       #count no. of species and sort in order
  mutate(species = factor(species)) |> #convert species to a factor
  filter(n > 3)                        #keep only n > 3 species
starwars_clean
levels(starwars_clean$species) #check factor levels, still has all species

#droplevels() - Remove unused levels (always!)
starwars_clean <- starwars |>
  filter(!is.na(species)) |>
  count(species, sort = TRUE) |>
  mutate(species = factor(species)) |>
  filter(n > 3) |> 
  droplevels() #remove unused levels
levels(starwars_clean$species) #now only has the two filtered levels

#fct_recode() - Rename levels
starwars_clean <- starwars |>
  filter(!is.na(species)) |>
  count(species, sort = TRUE) |>
  mutate(species = factor(species)) |>
  filter(n > 3) |> 
  droplevels() |> 
  mutate(species = fct_recode(species, "Humanoid" = "Human")) #column, new, old
starwars_clean

## Fun package! Generates music notation and audio directly from R!
# Also requires MuseScore — see https://flujoo.github.io/gm/ for setup
m <- 
  Music() +                       #initialize a Music object
  Meter(4,4) +                    #4/4 time signature
  Line(c("C5", "D5", "E5", "F5")) #four quarter notes

gm::show(m, to = c("score", "audio")) #renders sheet music + plays audio