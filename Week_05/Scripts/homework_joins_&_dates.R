### Data wrangling: joins & dates with lubridate
### Created by: Wei Shen Lim
### Created on: 2026-09-22
###########################################################################

## Load Libraries
library(tidyverse)
library(calecopal)
library(here)

## Load depth data 
DepthData <- read_csv(here("Week_05", "Data", "DepthData.csv"))
str(DepthData) #date data already in POSIXct format

## Load conductivity data, convert date column, round to nearest 10 secs, then join to depth data
## Calculate averages of date, depth, temperature, and salinity by minute
## Make a plot using the averaged data
Cond_DepthData <- read_csv(here("Week_05", "Data", "CondData.csv")) |> 
  mutate(date = mdy_hms(date)) |> #naming the new column as the old column to replace it, convert date column to POSIXct format
  mutate(date = round_date(date, "10 seconds")) |> #round to nearest 10 secs to match depth data
  inner_join(DepthData) |> #join with depth data
  mutate(date_min = round_date(date, "minute")) |> #rounding to nearest minute
  group_by(date_min) |> #calculate averages by minute
  summarise(mean_depth = mean(Depth, na.rm = TRUE),
            mean_temp = mean(Temperature, na.rm = TRUE),
            mean_salinity = mean(Salinity, na.rm = TRUE)) |> 
  pivot_longer(cols = mean_depth:mean_salinity, #pivot wide data to long to easily facet wrap by parameters
               names_to = "mean_params",
               values_to = "values") |> 
  ggplot(aes(x = date_min,
             y = values,
             color = mean_params)) +
  geom_point(size = 2) + #plotting time series
  geom_line(size = 1) + #plotting time series
  facet_wrap(~mean_params, ncol = 1, scales = "free", #wrap by the 3 parameters
             labeller = as_labeller(c("mean_depth" = "Mean Depth (m)", #changing the labels
                                      "mean_salinity" = "Mean Salinity (ppm)",
                                      "mean_temp" = "Mean Temperature (C)"))) +
  theme_bw() + #bw theme to keep it clean
  labs(title = "Time series of mean depth, salinity, and temperature on 2021-01-15",
       x = NULL, #taking out x-axis label cause it would just be the date, and it's already on the title
       y = NULL) + #taking out y-axis label, each plot labelled by facet_wrap
  guides(color = "none") + #remove color legend
  theme(plot.title = element_text(size = 20), #change size of different text elements
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 15),
        plot.caption = element_text(size = 12) ) +
  scale_color_manual(values = cal_palette("superbloom3")) #set color scale from "calecopal"
Cond_DepthData

## Save output of the plot
ggsave(here("Week_05", "Output", "homework_joins_&_dates.png"), Cond_DepthData,
       width = 12, height = 10)