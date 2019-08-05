# Datasets

There are two locations, at the moment, containing datasets:
1) Within this repository, under `MortalityMinder/data`. This data has been treated by loaders to filter unnecessary columns out, and rename other columns in preparation for binding. This data is being used for the current version of MortalityMinder (currently 0.01 Beta).
2) Within the shared `data` folder on the "cluster". This can be found in `~/data/AHRQ_Challenge/Datasets` (assuming you have your symbolic link set up (ask John for help if you do not) ). These datasets were collected by all members of the team, but are not necessarily treated by loaders (unless they exist in the first location as well).

When searching for data to create visualizations / conclusions, take note of three things:
1) The data you need may already exist in either of those two locations. This will save you time finding data, save the Infrastructure Team time from adding new datasets, and save overall storage space. It will also maintain uniformity in data (we wouldn't want two identical CDC datasets but with modifications in column names/order.)
2) The data may already be treated by a loader. If that's the case, _especially_ do not download the same dataset and modify it. That'll mean we break your visualization, or we have to write a new loader.
3) If you'd like to use a new dataset, and want to see it implemented in MortalityMinder, do one of two things:
  1) (easy) Contact the infrastructure team in person or by Slack to add this. If you have code extracting the data and putting it into a dataframe or the like, notify us as well. That'll help us in creating the loader.
  2) (intermediate) Create a pull request to this repo with the new datasets. If you don't know how to, no need to try; do point 1.
  
There is an Excel spreadsheet detailing each of the datasets that are in use by MortalityMinder and its loaders:
1) The source of the dataset
2) A category, if applicable
3) Name of the dataset
4) file format (.txt, .rds, .csv, etc)
5) Column names, types, units of measurement (if applicable), whether the column is retained by the loaders, what it is converted to, and other notes.

__Loader:__ An R script that scans through one or multiple datasets and sythesizes the information into one or multiple dataframes for simpler R use.

__Dataset:__ You know this already. A file containing data relevant to our studies. Can be in various file formats and come in a table-like syntax.

__Repository:__ Where the code for the application is held for worldwide viewing (like you!). Using Git/GitHub, you can clone the code to your computer, make changes, and make a pull request to the repository, where contributors can go over your modifications and approve it.
