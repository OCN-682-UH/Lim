### Data wrangling: joins & dates with lubridate using data biogeochemistry data from Silbiger et al. 2020
### Created by: Wei Shen Lim
### Created on: 2026-09-22
##########################################################################

## Load Libraries
library(tidyverse)
library(here)
library(ggplot)
library(ggcats)

## Create datasets
T1 <- tibble(
  Site.ID = c("A", "B", "C", "D"), #characters have to be in quotes
  Temperature = c(14.1, 16.17, 15.3, 12.8) #numbers are fine by themselves
)

T2 <- tibble(
  Site.ID = c("A", "B", "D", "E"),
  pH = c(7.3, 7.8, 8.1, 7.9)
)

## Join both datasets(6 ways)
left_join(T1, T2) #keeps all rows from the left (first) dataframe and adds matching rows from the right (second) dataframe
right_join(T1, T2) #opposite, keeps all rows from the right (second) dataframe
inner_join(T1, T2) #keeps only rows that exist in both dataframes
full_join(T1, T2) #keeps all rows from both dataframes
semi_join(T1, T2) #keeps rows from the first dataframe where there are matches in the second, but only return columns from the first
anti_join(T1, T2) #returns rows in the first dataframe that do not match the second

## Create dataset with different column name
T3 <- tibble(
  SiteID = c("A", "B", "C", "D"),
  Chlorophyll = c(2.3, 3.1, 1.9, 2.8)
)

## Join datasets using the 'by' argument - for diferent column names
left_join(T1, T3, by = c("Site.ID" = "SiteID")) #saying that the two columns are the same despite the different names

## Create more datasets to join by multiple columns
T4 <- tibble(
  Site.ID = c("A", "A", "B", "B"),
  Year = c(2020, 2021, 2020, 2021),
  Biomass = c(12.5, 15.3, 18.2, 16.9)
)

T5 <- tibble(
  SiteID = c("A", "A", "B"),
  Year = c(2020, 2021, 2021),
  Nutrients = c(8.2, 7.9, 9.1)
)

## Must still include the 'by' argument when joining by multiple columns
left_join(T4, T5, by = c("Site.ID" = "SiteID", "Year" = "Year"))

## Create more dataset to handle name conflicts
T6 <- tibble(
  Site.ID = c("A", "B", "C"),
  Notes = c("pristine", "degraded", "moderately impaired")
)

T7 <- tibble(
  Site.ID = c("A", "B", "D"),
  Notes = c("sunny", "shaded", "partially shaded"),
  Quality = c("good", "fair", "poor")
)

left_join(T6, T7, by = "Site.ID") #both datasets have "Notes" but you don't want to combine them, this is bad practice

## Best practices is to rename columns before joining
T6_renamed <- T6 |> 
  rename(Condition_Notes = Notes)

T7_renamed <- T7 |> 
  rename(Habitat_Notes = Notes)

left_join(T6_renamed, T7_renamed, by = "Site.ID")

##########################################################################
## Dates: What date and time is it now?
now() #good for time stamping
today() #just the date

## Current time in different timezones
now(tzone = "EST") #East Coast
now(tzone = "GMT") #Greenwich Mean Time
today(tzone = "GMT") #just the date in GMT
now(tzone = "US/Hawaii") #Hawaii Time

## Time checks
am(now()) #is it morning?
leap_year(now()) #is it a leap year?

## Convert dates to ISO format (YYYY-MM-DD)
#ensure data column is character, not a factor, use 'as.character()' if needed.
ymd("2021-02-24") #ISO format
mdy("02/24/2021") #US format
mdy("February 24 2021") #Written month
dmy("24/02/2021") #European format

## Convert date and time to ISO format with military time
ymd_hms("2021-02-24 10:22:20 PM") #ISO format with time
mdy_hms("02/24/2021 22:22:20") #US format with time
mdy_hm("February 24 2021 10:22 PM") #Written month with hour/min
#use "hm" if you don't have seconds

## Create a vector of datetimes
datetimes <- c(
  "02/24/2021 22:22:20", #all in quotes cause they have to be character
  "02/25/2021 11:21:10",
  "02/26/2021 8:01:52"
)
datetimes

## Convert the bector to datetime objects
datetimes <- mdy_hms(datetimes)
datetimes #now output format is POSIXct!

## Extract month (as number)
month(datetimes)
## Extract month (as abbreviated label)
month(datetimes, label = TRUE)
## Extract month (as full name)
month(datetimes, label = TRUE, abbr = FALSE)

## Extract day of the month
day(datetimes)
## Extract day of the week
wday(datetimes, label = TRUE)
## Extract hour, minute, second
hour(datetimes)
minute(datetimes)
second(datetimes)

## Adding time intervals
#singular extracts the component, plural adds component to a datetime
datetimes + hours(4) #add 4 hours
datetimes + days(2) #add 2 days
datetimes + months(1) #add 1 month

## Rounding dates
round_date(datetimes, "minute") #round to nearest minute
round_date(datetimes, "5 mins") #round to nearest 5 minutes

## Create a datetime WITHOUT timezone info
datetime_naive <- mdy_hms("02/24/2021 10:22:20")
datetime_naive #this has no timezone information, bad!

## View same moment in different timezone
hawaii_time <- with_tz(datetime_naive, tzone = "US/Hawaii")
hawaii_time #convert the same time to hawaii time zone
## Same moment, viewed from EST
est_time <- with_tz(hawaii_time, tzone = "EST")
est_time
#instant in time is the same, just viewing in different time zones

## Change time zone label
force_hawaii <- force_tz(datetime_naive, tzone = "US/Hawaii")
force_hawaii #change the timezone of this object to hawaii, not just convert it 
## Now convert to EST (this changes the clock time)
with_tz(force_hawaii, tzone = "EST")

## Challenge
## Read in conductivity data and convert date column to a datetime
CondData <- read_csv(here("Week_05", "Data", "CondData.csv")) |> 
  mutate(datetime = mdy_hms(date))
View(CondData)

## Read in data and join them
SiteData <- read_csv(here("Week_05", "Data", "site.characteristics.data.csv")) |> 
  pivot_wider(names_from = parameter.measured,
              values_from = values)
ToptData <- read_csv(here("week_05", "Data", "Topt_data.csv"))
JoinedData <- left_join(SiteData, ToptData)

## Fun package! Plot with cats in ggplot2
df <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 3, 5, 4)
)

ggplot(df, aes(x, y)) +
  geom_cat(cat = "nyancat",
           size = 4) +
  labs(title = "Cats!",
       x = "X", y = "Y") +
  theme_minimal()