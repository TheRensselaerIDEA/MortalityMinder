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

## CHR Determinent Selection

CHR Data has now been mostly parsed and is now available for interpretation and visualization. To access it and utilize its functions:
1) Go to `beta.01>init` and run all of `Librarian.R` if you have not yet.
2) Run all of `Loader_CHR_General.R`.
3) You can view data for each year by observing `chr.data.<year>` in your Environment. The data is stored as a list of lists. To retrieve a specific value, write in your Console `chr.data.<year>[[<determinant>]][[<statistic>]][<county index>]`.
3a) To retrieve indices for counties, call `chr.indices()` to return `chr.indices.df`, a data frame containing in each row a set of county name, state name, and FIPS. The index of this row corresponds to the element in this index of a list within the greater list (if that makes no sense, just plug it into the code in 3) and understand it matches with the original dataset).
4) To retrieve a data frame containing data for a specific determinant, use `chr.selector(<determ>, <start year>, <end year>)`. This will store the result in `chr.determ.output`, which you can rename for your purposes.
5) The selector function only returns for a specific determinant. If you want your data frame to contain multiple determinants, call this function multiple times, inputting the different determinants, and merge them together. We give you this freedom in order for you to have control over how much data size you're willing to have.
