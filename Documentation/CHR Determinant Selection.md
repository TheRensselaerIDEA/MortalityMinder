
![Good news, everyone](https://media.giphy.com/media/3o7abA4a0QCXtSxGN2/giphy.gif)

CHR Data has now been mostly parsed and is now available for interpretation and visualization. To access it and utilize its functions:
1) Go to `beta.01>init` and run all of `Librarian.R` if you have not yet.
2) Run all of `Loader_CHR_General.R`.
3) You can view data for each year by observing `chr.data.<year>` in your Environment. The data is stored as a list of lists. To retrieve a specific value, write in your Console `chr.data.<year>[[<determinant>]][[<statistic>]][<county index>]`.
3a) To retrieve indices for counties, call `chr.indices()` to return `chr.indices.df`, a data frame containing in each row a set of county name, state name, and FIPS. The index of this row corresponds to the element in this index of a list within the greater list (if that makes no sense, just plug it into the code in 3) and understand it matches with the original dataset).
4) To retrieve a data frame containing data for a specific determinant, use `chr.selector(<determ>, <start year>, <end year>)`. This will store the result in `chr.determ.output`, which you can rename for your purposes.
5) The selector function only returns for a specific determinant. If you want your data frame to contain multiple determinants, call this function multiple times, inputting the different determinants, and merge them together. We give you this freedom in order for you to have control over how much data size you're willing to have.

As usual, message the Infrastructure team on Slack if there are any questions.
