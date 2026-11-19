### Iterative Coding
### Created by: Wei Shen Lim
### Created on: 2026-11-17
##########################################################################

### Load Libraries
library(tidyverse)
library(here)
library(gganatogram)

############################ for loops Template ###############################
for (index in sequence) {
  command to repeat
}

########################## Simple for loop Example ############################
# Ask R to print statement telling us what year it is along a sequence of years
print(paste("The year is", 2000))

# Put it in a for loop
years <- c(2015:2021)

for (i in years) {               #i is the index
  print(paste("The year is", i)) #loop over i
}

########################### Save for loop Output ##############################
# Pre-allocate space: empty columns as long as the years vector
#Pre-allocating is let's R do it faster than just using c()
year_data <- tibble(year = rep(NA, length(years)),
                    year_name = rep(NA, length(years)))
year_data

# Add the for loop
#Use seq_along, not length(x)
for (i in seq_along(years)) {
  year_data$year_name[i] <- paste("The year is", years[i])
  year_data$year[i] <- years[i]
}
year_data

################## Use loops to read in multiple .csv files ###################
# There are 3 files total, look at one to see what it looks like
testdata <- read_csv(here("Week_13", "Data", "cond_data", "011521_CT316_1pcal.csv"))
glimpse(testdata)

# List files in a directory
# Point to the folder on the computer
CondPath <- here("Week_13", "Data", "cond_data")

# List all files in that path matching a pattern (here, anything with .csv)
# You can use regex to be more specific about filenames
files <- dir(path = CondPath, pattern = ".csv")
files

# Pre-allocate space for the loop
# One row for each file and 3 columns
cond_data <- tibble(filename = rep(NA, length(files)),
                    mean_temp = rep(NA, length(files)),
                    mean_sal = rep(NA, length(files)))
cond_data

# Write the basic code first
# Test by reading in the first file
raw_data <- read_csv(paste0(CondPath, "/", files[1]))
head(raw_data)

# Calculating mean temp, will be the same for mean sal
mean_temp <- mean(raw_data$Temperature, na.rm = TRUE)
mean_temp

# Turn it into a for loop
# Add in the loop over the raw data
for (i in seq_along(files)) { #loop over each file (1, 2, 3)
  raw_data <- read_csv(paste0(CondPath, "/", files[i])) #change 1 to i
  glimpse(raw_data)           #remove after
}

# Add the filename for each row
for (i in seq_along(files)) {
  raw_data <- read_csv(paste0(CondPath, "/", files[i]))
  
  cond_data$filename[i] <- files[i]
}
cond_data

# Add in means
for (i in seq_along(files)) {
  raw_data <- read_csv(paste0(CondPath, "/", files[i]))
  
  cond_data$filename[i] <- files[i]
  cond_data$mean_temp[i] <- mean(raw_data$Temperature, na.rm = TRUE)
  cond_data$mean_sal[i] <- mean(raw_data$Salinity, na.rm = TRUE)
}
cond_data

################################### purrr #####################################
# Same thing but with tidyverse style language
# uses map() function

############################### Simple Example ################################
# Calculate the mean of a set of random numbers, 10 times

# Option 1: use a canned function that already exists
set.seed(3817) #makes the random numbers reproducible
1:10 |>                 #the vector to iterate over
  map(rnorm, n = 15) |> #15 random numbers from a normal distribution, in a list
  map_dbl(mean)         #the result is not a vector of type "double"

# Option 2: make your own function
set.seed(3817)
1:10 |> 
  map(function(x) rnorm(15, x)) |> #make your own function
  map_dbl(mean)

# Option 3: Use a formula
set.seed(3817)
1:10 |> 
  map(~ rnorm(15, .x)) |> #changes the arguments inside the function
  map_dbl(mean)

############## Read in files using purrr instead of a for loop ################
CondPath <- here("Week_13", "Data", "cond_data")
files <- dir(path = CondPath, pattern = ".csv")
files

# Or get the full file names in one less step
files <- dir(path = CondPath, pattern = ".csv", full.names = TRUE) #saves the entire path
files

# Use map() to read in files
# Then stack list of dataframes into one with list_rbind()
# Keeping filename as a column
data <- files |> 
  set_names() |>                    #names each list element with its file name
  map(read_csv) |>                  #a list of 3 dataframes
  list_rbind(names_to = "filename") #stack them; list names go into a "filename" column
data
# Might see map_df() in older code, but now uses map() + list_rbind() instead

# Calculate means
# Using group_by() filename and summarise()
data <- files |> 
  set_names() |> 
  map(read_csv) |> 
  list_rbind(names_to = "filename") |> 
  group_by(filename) |> 
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity, na.rm = TRUE))
data

################## Shortcut: readr can read many files at once #################
# read_csv() accepts a vector of file paths and stacks them for you
# Only works when files share the same columns
data <- read_csv(files, id = "filename") |> 
  group_by(filename) |> 
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity, na.rm = TRUE))
data

############# Make a plot from each file and save it automatically ############
# Using (purrr)
file_names <- dir(path = CondPath, pattern = ".csv") #just the file names

save_temp_plot <- function(file_name) {
  site_name <- str_remove(file_name, "\\.csv") #drop ".csv" for the title
  
  p <- read_csv(paste0(CondPath, "/", file_name)) |> 
    ggplot(aes(x = date, y = Temperature)) +
    geom_line() +
    labs(title = site_name, x = "Date", y = "Temperature (°C)")
  
  ggsave(here("Week_13", "Output", paste0(site_name, "_temp.png")),
         plot = p, width = 6, height = 4)
}

# Use walk() instead of map() for function's side effect (saving a file, printing)
# rather than a returned value
walk(file_names, save_temp_plot) #files, function

# Using (for loop) - slower
file_names <- dir(path = CondPath, pattern = ".csv") #just the file names

for (i in seq_along(file_names)) {
  raw_data <- read_csv(paste0(CondPath, "/", file_names[i]))
  
  site_name <- str_remove(file_names[i], "\\.csv") #drop ".csv" for the title
  
  p <- ggplot(raw_data, aes(x = date, y = Temperature)) +
    geom_line() +
    labs(title = site_name, x = "Date", y = "Temperature (°C)")
  
  ggsave(here("Week_13", "Output", paste0(site_name, "_temp.png")),
         plot = p, width = 5, height = 4)
}

# Check the output folder
dir(here("Week_13", "Output"), pattern = "_temp.png")

### Use R for anatomy!!
# Female anatomy
gganatogram(data = hgFemale_key,
            organism = "human", sex = "female",
            fill = "colour", fillOutline = "#a6bddb") +
  theme_void() +
  coord_fixed()

# Just the nervous system
hgMale_key |> 
  filter(type %in% "nervous_system") |> 
  gganatogram(organism = "human", sex = "male",
              fill = "colour", outline = FALSE) +
  theme_void() +
  coord_fixed()

# A mouse
gganatogram(data = mmFemale_key,
            organism = "mouse", sex = "female",
            fillOutline = "#a6bddb", fill = "colour") +
  theme_void() +
  coord_fixed()

# A cell
gganatogram(data = cell_key$cell,
            organism = "cell",
            fillOutline = "#a6bddb", fill = "colour") +
  theme_void() +
  coord_fixed()