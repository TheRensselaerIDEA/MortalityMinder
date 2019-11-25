# Overall Description

The `init` folder is consisting of all of the scripts for app initialization. It
is mainly consisted of three components:

* Package Loader (Librarian.R)
* Data Loader
* Function Loader

All of the related scripts will be called from `Source.R`

## 1. Package Loader

`Librarian.R` is the only script for this package management objective.

### Librarian.R

The package loader is used for managing packages used by all other components of the app. Because of the prone-to-conflict nature of `R`, we tend to `require()` or `install.packages()` inside a single file. It is better for conflict management: because loading order decides which function is actually called (when repeated function names encountered), and it is better for debugging. Also, a simple *auto installation* mechanism is used for convenience.

## 2. Data Loader

Any scripts starts with `Loader_` is a data file loader, e.g. `Loader_CDC.R`. These scripts have two objectives:

* Loading raw data files like `.txt` or `.csv` to R data type `.rds`, either in the form of `list`, `data.frame` or `tibble`.
* Unify data types and common variable names.

### Loader_CDC.R & Loader_CDC_imputation.R

Both of these two scripts are used for loading CDC mortality data. The imputation version is used to deal with missing values. The output should consist of a single data frame containing mortality data for all counties, all periods (six periods in total), all causes of deaths.

### Loader_CHR.R & Loader_CHR2019.R

Both of these two files are used for loading data set from county heath ranking. `Loader_CHR.R` is an attempt for loading *ALL Years* while the 2019 version is dedicated to 2019 data.

### Loader_RHI.R

This script is used to load some demographics data from RHI Hub. Factors like education level and median household income are loaded.

### Loader_GEO.R

This script is used to load all related geographic information, such as latitudes and longitudes of counties, into the working environment. These data sets are used for plotting maps.

## 3. Function Loader

Any scripts starts with `Analyzer_` is a function loader, e.g. `Analyzer_PCA.R`.

### Analyzer_PCA.R

This script is used for loading helper functions for doing PCA and associated visualizations in the app.

## 4. Others

`Theme.R` is used for unifying the ggplot themes.
