## NBA Play-by-Play Subsetting (Summary/Plot Producing) App

**Author**: Chandler Weedon **Date**: November 2025 **ST558 - Project 2**

## Overview

This Shiny app allows users to subset, view, and explore NBA Play-by-Play data from the 2017–2018 seasons. The data include variables related to scoring and shooting such as shot type, shot outcome, shot distance, and points. Users can filter by quarter, shot outcome, and numeric variables like shot distance and total points.

## How to Use

-   Use the sidebar to select categorical and numeric filters, the click Apply Filters to update the app.
-   **About Tab**: Describes the app, data source, and tab functions.
-   **Data Download Data Tab**: Displays the filtered data and allows download of a .csv file.
-   **Data Exploration Tab**: Show summaries and plots of categorical and numeric variables.

## Data Source

-   NBA Play-by-Play data (2016-2021) obtained from kaggle (<https://www.kaggle.com/datasets/schmadam97/nba-playbyplay-data-20182019>) after being scraped from Basketball Reference. **Condensed to 2017-2018 for analysis.**

## Packages Used
tidyverse, shiny, DT

##Running the App (Copy and paste into R-Code Block)
Open app.R and run shiny::runApp() in console.