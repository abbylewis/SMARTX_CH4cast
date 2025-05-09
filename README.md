# SMARTX CH<sub>4</sub>cast

## Summary

Forecasts of methane (CH<sub>4</sub>) fluxes from the Salt Marsh Accretion Response to Temperature eXperiment (SMARTX). SMARTX is a long-term whole-ecosystem in situ warming experiment located within the Global Change Research Wetland (GCReW; Edgewater, MD, USA; 38º53′ N, 76º33′ W). Here, we generated six-month-ahead forecasts of CH<sub>4</sub> fluxes and assessed forecasts throughout 2024.

## Personnel

-   Abigail Lewis, Smithsonian Environmental Research Center, abigail.sl.lewis\@gmail.com

## Repository structure

To recreate figures in the manuscript, first refer to the files in `./analysis/`, which include ordered scripts to score all forecasts, generate figures, and calculate statistics for the manuscript text.

Other files in the repository were used to generate forecasts throughout 2024, as described below:

-   `./analysis/`: All data analysis and figure generation scripts

-   `./Raw_data/`: Unprocessed data inputs, including meteorological forecasts and experimental metadata

-   `./R/`: Utility scripts for forecast generation and scoring

-   `./outputs/`: Forecast outputs. Each file includes forecasts generated on one date using a given model. Files with the designation "comb_reps" are forecasts of the mean CH<sub>4</sub> flux across all plots in a given treatment

-   `./models/`: Code for each of 10 forecast models. Each model-specific subdirectory includes model-specific code `forecast_model.R`, as well as simple wrapper functions to run or re-run forecasts

-   `./met_downloads/`: Formatted meteorological driver data

-   `L1_target.csv`: Formatted CH<sub>4</sub> flux data

-   `forecast_variables.csv`: Variable description for forecast target variable (CH<sub>4</sub> flux)
