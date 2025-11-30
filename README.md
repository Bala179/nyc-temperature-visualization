# NYC Temperature Visualization

Work in progress by Balaganesh Manikandan. 2025

## Project description

This project aims to create an interactive R Shiny temperature visualization app for NYC to illustrate local temperature trends in the city. For further details, please refer to [this](https://github.com/sparklabnyc/ERA5-Land-grids-into-national-subnational-boundaries-worldwide) repository.

This is an R Shiny app that has been deployed on Posit. It can be viewed [here](https://019ab86a-ef53-6955-1e1f-dedf58b536b8.share.connect.posit.cloud/).

## 1. Data

Folder containing data used in analysis:
a. `nyc_extracted_temps.csv`: Contains zipcode-wise hourly temperatures in NYC from 2020 onwards. Temperatures have been recorded at intervals of 3 hours. All dates and times are in UTC.
b. `nyc_daily_XXX.csv` (`XXX` can be `avg`, `min`, or `max`): Contains daily average, minimum, and maximum zipcode-wise temperatures for each day in NY local time.

## 2. Code

note: please run `create_folder_structure.R` first to create folders which may not be there when first loaded

`app.R`: Contains main code for R Shiny app.

## How to Update Data

1. Clone this repository (first time only): `git clone https://github.com/Bala179/nyc-temperature-visualization.git`
2. Follow the instructions in [this](https://github.com/sparklabnyc/ERA5-Land-grids-into-national-subnational-boundaries-worldwide) repository to download and process new NYC data that was not available before. (Note: to merge this with the existing data, copy and paste the data files from this repository into the `data` folder of the other repository.)
3. You can test your app out locally by running `Rscript -e "shiny::runApp()"`.
4. Once the new files have been generated, copy and paste them back to this repository, and then commit and push your changes.
5. The app will automatically reload once a push is received, and the new data should be visible in the app.

