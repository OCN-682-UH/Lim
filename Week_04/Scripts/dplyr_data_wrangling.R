### Data wrangling with dpylr using data from the "palmerpenguins" package
### Created by: Wei Shen Lim
### Created on: 2026-09-15
##########################################################################

### Load Libraries
library(palmerpenguins)
library(tidyverse)
library(here)
library(dadjokeapi) #DAD JOKES! with groan()

### View data
view(penguins)
head(penguins)

## Filter data 
#Select only females
filter(.data = penguins, sex == "female")

#Select only penguins measured in 2008 (numbers do not need to be in quotations)
filter(.data = penguins, year == 2008)

#Select only body mass greater than 5000
filter(.data = penguins, body_mass_g > 5000)

## Boolean operators
#Select females that are also greater than 5000g
filter(.data = penguins, sex == "female", body_mass_g > 5000)
filter(.data = penguins, sex == "female" & body_mass_g > 5000) #Boolean operators!

#Select penguins collected in either 2008 or 2009
filter(.data = penguins, year > 2007)
filter(.data = penguins, year == 2008 | year == 2009) #Boolean operators!
filter(.data = penguins, year %in% c(2008, 2009))

#Select penguins not from the island Dream
filter(.data = penguins, island != "Dream")
filter(.data = penguins, !island == "Dream")

#Select penguins in the species Adelie and Gentoo
filter(.data = penguins, species != "Chinstrap")
filter(.data = penguins, species %in% c("Adelie", "Gentoo"))

## Mutate
mutate(.data = penguins,
       body_mass_kg = body_mass_g / 1000,
       bill_length_depth = bill_length_mm / bill_depth_mm)

## Mutate multiple columns at once
#For every numeric column, round its values to 1 decimal place
penguins |>
  mutate(across(where(is.numeric), ~round(.x, 1)))

## Mutate with if_else
#If the year is > 2008, then it'll be "After 2008", if not it's "Before 2008"
mutate(.data = penguins,
       after_2008 = if_else(year > 2008, "After 2008", "Before 2008"))

## Create new column to add flipper length and body mass together
mutate(.data = penguins,
       flipper_body = flipper_length_mm + body_mass_g)

## Create new column where body mass > 4000 is "Big", everything else is "Small"
mutate(.data = penguins,
       size = if_else(body_mass_g > 4000, "Big", "Small"))

#Combine both
mutate(.data = penguins,
       flipper_body = flipper_length_mm + body_mass_g,
       size = if_else(body_mass_g > 4000, "Big", "Small"))

## Pipe Operator |> "Shift + Ctrl + M"
#Filter only female penguins and add new column that calculates the log body mass
penguins |> 
  filter(sex == "female") |> 
  mutate(log_mass = log(body_mass_g))

## Select certain columns to remain in the dataframe
penguins |> 
  filter(sex == "female") |> 
  mutate(log_mass = log(body_mass_g)) |> 
  select(species, island, sex, log_mass)

## Use select() to rename columns
#Here we're renaming species to have a capital S
penguins |> 
  filter(sex == "female") |> 
  mutate(log_mass = log(body_mass_g)) |> 
  select(Species = species, island, sex, log_mass)

## Arrange to sort rows by a column - ascending by default
penguins |> 
  arrange(body_mass_g)

## Use desc() to sort in descending order
penguins |> 
  arrange(desc(body_mass_g))

## Summarize, compute a table of summarized data
#Calculate the mean flipper length (and exclude any NAs)
penguins |> 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE))

#Calculate mean and min flipper length
penguins |> 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
            min_flipper = min(flipper_length_mm, na.rm = TRUE))

## Summarize values by certain groups.
#Calculate mean, max, and count of bill length by island
penguins |> 
  group_by(island) |> 
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm = TRUE),
            n = n()) #n() counts the number of rows in each group

#Group by both island and sex
penguins |> 
  group_by(island, sex) |> 
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = mean(bill_length_mm, na.rm = TRUE))

## count() is a quick shortcut for counting rows per group - no need for group_by() + summarise()
penguins |> 
  count(species)

#count by multiple variables at once
penguins |> 
  count(species, island)

## Remove NAs
penguins |> 
  drop_na(sex)

#Drop all rows with NA on sex, then calculate mean bill length by island and sex
penguins |> 
  drop_na(sex) |> 
  group_by(island, sex) |> 
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

## Pipe data wrangling into a ggplot
penguins |> 
  drop_na(sex) |> 
  ggplot(aes(x = sex,
             y = flipper_length_mm)) +
  geom_boxplot()
