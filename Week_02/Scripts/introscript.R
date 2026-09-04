### Learning how to input data with 'here'
### Created by: Wei Shen Lim
### Created on: 2026-09-04
###########################################

### Load Libraries
library(here)
library(tidyverse)

### Read in Data
WeightData <- read.csv(here("Week_02","Data","weightdata.csv"))

### Data Analysis
head(WeightData) #always inspect data
tail(WeightData)
view(WeightData)