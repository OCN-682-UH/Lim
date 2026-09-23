### MBIO612 / OCN682: Data Science Fundamentals in R (Fall 2026)
___
This repository contains my assignments for this course, updated weekly.

Week 5 update~

#### Contents

* [*Week_02*](https://github.com/OCN-682-UH/Lim/tree/main/Week_02)  
  * first script to test out `here`, reading in 'weightdata.csv'
* [*Week_03*](https://github.com/OCN-682-UH/Lim/tree/main/Week_03)  
  * Intro to Plotting, ggplot with data from the "palmerpenguins" package
* [*Week_04*](https://github.com/OCN-682-UH/Lim/tree/main/Week_04)  
  * Data Wrangling with dplyr and tidyr
* [*Week_05*](https://github.com/OCN-682-UH/Lim/tree/main/Week_05)
  * Data Wrangling: joins & dates with lubridate

##### About me:
I'm a Marine Biology MS student in the [Sherwood Algal Biodiversity Lab](https://sherwoodalgalbiodiversitylab.weebly.com/) at UH Mānoa. I study the systematics and taxonomy of *Bryopsis* (Bryopsidales, Chlorophyta) from shallow and mesophotic habitats of the Hawaiian Islands.

![Bryopsis](https://coe.hawaii.edu/opihi/wp-content/uploads/sites/25/2023/01/opihi_organism_id_29.jpg)
*Credit: OPIHI UH Mānoa*

That's it from me for now~

___
#### Repository Structure & Contents:
```text
├── README.md
|
├── Week_02
│   ├── Data
│   │   └── weightdata.csv
│   └── Scripts
│       └── introscript.R
|
├── Week_03
|   ├── Output
|   │   ├── homework_ggplot.png
|   │   └── penguin.png
|   └── Scripts
|       ├── ggplot_penguins_1.R
|       ├── ggplot_penguins_2.R
|       └── homework_ggplot.R
|       
├── Week_04
|   ├── Data
|   │   ├── chem_data_dictionary.csv
|   │   └── chemicaldata_maunalua.csv
|   ├── Output
|   │   ├── homework_dplyr.png
|   │   ├── homework_tidyr.png
|   │   ├── hw_summary.csv
|   │   └── summary.csv
|   └── Scripts
|       ├── dplyr_data_wrangling.R
|       ├── homework_dplyr.R
|       ├── homework_tidyr.R
|       └── tidyr_data_wrangling.R
|
├── Week_05
|   ├── Data
|   │   ├── CondData.csv
|   │   ├── DepthData.csv
|   │   ├── Topt_data.csv
|   │   ├── data_dictionary.csv
|   │   └── site.characteristics.data.csv
|   ├── Output
|   │   └── homework_joins_&_dates.png
|   └── Scripts
|       ├── homework_joins_&_dates.R
|       └── joins_&_dates_data_wrangling.R