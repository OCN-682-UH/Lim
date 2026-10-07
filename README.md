### MBIO612 / OCN682: Data Science Fundamentals in R (Fall 2026)
___
This repository contains my assignments for this course, updated weekly.

Week 7 update~

#### Contents

* [*Week_02*](https://github.com/OCN-682-UH/Lim/tree/main/Week_02)  
  * first script to test out `here`, reading in 'weightdata.csv'
* [*Week_03*](https://github.com/OCN-682-UH/Lim/tree/main/Week_03)  
  * Intro to Plotting, ggplot with data from the "palmerpenguins" package
* [*Week_04*](https://github.com/OCN-682-UH/Lim/tree/main/Week_04)  
  * Data Wrangling with dplyr and tidyr
* [*Week_05*](https://github.com/OCN-682-UH/Lim/tree/main/Week_05)
  * Data Wrangling: joins & dates with lubridate, and advanced plotting
* [*Week_06*](https://github.com/OCN-682-UH/Lim/tree/main/Week_06)
  * Introduction to Quarto: making figures and tables, with cross-referencing
  * intro_to_quarto.html = [HTML Output](https://01a0efb6-8f1b-00a4-3f7c-5746fb7e6a8a.share.connect.posit.cloud/)
  * quarto_part_2.html = [HTML Output](https://01a0f00d-2a3c-72c5-f89c-14521f6d9162.share.connect.posit.cloud/)
  * homework_quarto.html = [HTML Output](https://01a0f08c-3111-e92c-078e-b3f56b76e8f2.share.connect.posit.cloud/)
* [*Week_07*](https://github.com/OCN-682-UH/Lim/tree/main/Week_07)
  * Introduction to Maps: building interactive maps
  * homework_maps.html = [HTML Output](https://01a11514-3b5f-3025-c175-dfa66464bb1e.share.connect.posit.cloud/)

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
|   │   ├── homework_joins_&_dates.png
|   │   ├── penguin_animation.gif
|   │   └── penguinplot.png
|   └── Scripts
|       ├── advanced_plotting.R
|       ├── homework_joins_&_dates.R
|       └── joins_&_dates_data_wrangling.R
|       
├── Week_06
|   ├── Data
|   │   ├── chem_data_dictionary.csv
|   │   └── chemicaldata_maunalua.csv
|   ├── Output
|   │   ├── fig-correlation-plot-1.png
|   │   ├── fig-penguin-1.png
|   │   ├── homework_quarto.html
|   │   ├── intro_to_quarto.html
|   │   ├── quarto_part_2.html
|   │   └── meme-1.png
|   └── Scripts
|       ├── rsconnect/documents
|       ├── homework_quarto.qmd
|       ├── intro_to_quarto.qmd
|       └── quarto_part_2.qmd
├── Week_07
|   ├── Data
|   │   ├── tiles_cache/Esri.WorldImagery
|   │   ├── CAPopdata.csv
|   │   ├── chemicaldata_maunalua.csv
|   │   ├── meteorites.csv
|   │   └── stars.csv
|   ├── Output
|   │   ├── CApop.pdf
|   │   ├── homework_maps.html
|   │   └── maunalua_map.pdf
|   └── Scripts
|       ├── rsconnect/documents/homework_maps.qmd/connect.posit.cloud/weishen23
|       ├── homework_maps.qmd
|       ├── intro_to_maps_1.R
|       └── intro_to_maps_2.R