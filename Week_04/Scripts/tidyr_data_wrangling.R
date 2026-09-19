### Data wrangling with tidyr using data biogeochemistry data from Silbiger et al. 2020
### Created by: Wei Shen Lim
### Created on: 2026-09-19
##########################################################################

## Load Libraries
library(tidyverse)
library(here)
library(cowsay) #make your animals talk!

## Load data
ChemData <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
view(ChemData)

## Another way to remove NAs [drop_na() is the preferred approach]
ChemData_clean <- ChemData |> 
  filter(complete.cases(ChemData)) #filters out everything that is not a complete row

## Separate 2 bits of info from 1 column
ChemData_clean <- ChemData |> 
  drop_na() |> #remove all NAs
  separate_wider_delim(cols = Tide_time, #this is the new function to separate()
                       delim = "_", #this is the separator between the two info
                       names = c("Tide", "Time"), #always need to concatenate when you give more than 1 value
                       cols_remove = FALSE) #default removes the original column
head(ChemData_clean)

## Combine 2 columns in 1 with paste() with mutate() to add a new column for it
ChemData_clean <- ChemData |> 
  drop_na() |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide", "Time"),
                       cols_remove = FALSE) |> 
  mutate(Site_Zone = paste(Site, Zone, sep = ".")) #combine site and zone separated by "." onto new column called Site_Zone
head(ChemData_clean)

## Our data is wide format now, pivot to long format
ChemData_long <- ChemData_clean |> 
  pivot_longer(cols = Temp_in:percent_sgd, #select columns to pivot (all columns from Temp_in to percent_sgd)
               names_to = "Variables", #set the name of the old columns (header) to this new column
               values_to = "Values") #set the values of the old columns to this new column
head(ChemData_long)

#Calculate mean and variance for all variables at each site
ChemData_long |> 
  group_by(Variables, Site) |> 
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars = var(Values, na.rm = TRUE))

#Calculate mean, variance and standard variation for all variables by site, zone, and tide
ChemData_long |> 
  group_by(Variables, Site, Zone, Tide) |> 
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars = var(Values, na.rm = TRUE),
            Parm_sd = sd(Values, na.rm = TRUE))

## Facet wrap long data by every parameter by site
ChemData_long |> 
  ggplot(aes(x = Site,
             y = Values)) +
  geom_boxplot() +
  facet_wrap(~Variables, scales = "free_y") #facet_wrap() set fixed axes scales for all, not useful here, use scales = "" to set scale to free, if want both x- and y-axis to be free, just use "free"

## Pivot long data back to wide format
ChemData_wide <- ChemData_long |> 
  pivot_wider(names_from = Variables, #take the names from Variables column
              values_from = Values) #take the values from the Values column
head(ChemData_wide)

## Full pipeline with summary statistics and report
ChemData_clean <- ChemData |> 
  drop_na() |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide", "Time"),
                       cols_remove = FALSE) |> 
  pivot_longer(cols = Temp_in:percent_sgd,
               names_to = "Variables",
               values_to = "Values") |> 
  group_by(Variables, Site, Time) |> 
  summarise(mean_vals = mean(Values, na.rm = TRUE)) |> 
  pivot_wider(names_from = Variables,
              values_from = mean_vals) |> #make the averages wider
  write_csv(here("Week_04", "Output", "summary.csv"))

## Fun package! Make animals talk
say("I love tidy data!", by = "cat")