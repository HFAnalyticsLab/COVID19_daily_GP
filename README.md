# COVID19 daily GP data analysis

#### Project Status: [In progess]

## Project Description

A descriptive analysis of trends in daily GP activity. 

## Data source

We are using publically available data published by NHS digital. The data used for this analysis is not included in this repository but can be downloaded from their (website)[https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice/february-2020].

## How does it work?

The code can be used to download the data and replicate the analysis.

### Requirements

These scripts were written in R version version 3.6.3 (2020-02-29) -- "Holding the windsock" and RStudio Version 1.2.5033. 
The following R packages (available on CRAN unless otherwise specified) are needed: 
* [**tidyverse**](https://www.tidyverse.org/)(1.3.0)
* [**janitor**](https://cran.r-project.org/web/packages/janitor/index.html) (2.0.1)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)(1.7.8)
* [**testthat**](https://cran.r-project.org/web/packages/testthat/index.html)(2.3.2)
* **THFstyle** internal package


### Getting started

The 'R' folder contains:

* 0_download_data.R - download data.
* 1_clean_data.R - Load, clean and save cleaned data. 
* 2_tests.R - Unit tests to check that downloaded data is in the expected format 
* 3_coverage_plot.R - Create plot showing coverage
* 4_appointment_plots.R - Create appointment plots


## Authors

* **Emma Vestesson** - [@gummifot](https://twitter.com/gummifot) - [emmavestesson](https://github.com/emmavestesson)

## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/COVID19_daily_GP/blob/master/LICENSE).

