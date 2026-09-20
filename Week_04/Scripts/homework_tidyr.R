### Data wrangling with tidyr using data biogeochemistry data from Silbiger et al. 2020
### Created by: Wei Shen Lim
### Created on: 2026-09-19
##########################################################################

## Load Libraries
library(tidyverse)
library(here)

## Load data
ChemData <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
View(ChemData)

## Remove all NAs, separate Tide_time column, filter out a subset of data, pivot at least once
ChemData_clean <- ChemData |> 
  drop_na() |> #remove all NAs
  separate_wider_delim(cols = Tide_time, #separate Tide_time column into appropriate columns
                       delim = "_",
                       names = c("Tide", "Time"),
                       cols_remove = FALSE) |> 
  filter(Time == "Day") |> #filter to only include samples from day time
  pivot_longer(cols = Temp_in:percent_sgd, #pivot to long data for analysis
               names_to = "Variables",
               values_to = "Values")

## Calculate summary statistics and export using cleaned long data
ChemData_summary <- ChemData_clean |> #had to separate this out so I can use cleaned wide data for plotting
  group_by(Variables, Site, Zone, Season, Tide) |> #group by these categories for summary statistics
  summarise(Param_means = mean(Values, na.rm = TRUE), #calculate these summary statistics
            Param_vars = var(Values, na.rm = TRUE),
            Param_sd = sd(Values, na.rm = TRUE)) |> 
  write_csv(here("Week_04", "Output", "hw_summary.csv")) #export summary statistics

## Plot using cleaned wide data and export
ChemData_plot <- ChemData_clean |> 
  pivot_wider(names_from = Variables, #pivot back wide so I can use the temperature and salinity as my variables
              values_from = Values) |> 
  ggplot(aes(x = percent_sgd, #correlation between percent_sgd & salinity
             y = Salinity)) +
  geom_point() + #scatter plot
  geom_smooth(method = "lm") + #best fit line
  facet_wrap(~Season, ncol = 2) + #facet wrap by season
  theme_bw() + #bw theme to keep in clean but still have the grid lines
  theme(plot.title = element_text(size = 20), #change size of different text elements
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 15),
        plot.caption = element_text(size = 12) ) +
  labs(title = "Day time correlation between SGD % and salinity by season",
       x = "Submarine Groundwater Discharge (%)",
       y = "Salinity", #no unit for salinity in data dictionary
       caption = "Source: Silbiger et al. 2020 Proceedings of the Royal Society: B.")
ChemData_plot #try to categorize by zone with color but they all show similar overlapping best fit lines, seemed redundant to me

## Save output of the correlation plot
ggsave(here("Week_04", "Output", "homework_tidyr.png"), ChemData_plot, #clarify which plot
       width = 12, height = 10) #set size in inches
