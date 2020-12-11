# TidyTuesday

My Tidy Tuesday adventures: [https://hardin47.github.io/TidyTuesday/](https://hardin47.github.io/TidyTuesday/)


### Getting the data

Each week, copy the week's folder which includes both the data and the information describing the data.  Then create / navigate to the gh-pages branch where the .html file (from the .Rmd) describes the analysis.

1. Navigate to the R4DS folder to download

2. Modify the URL, replace `tree/master` with `trunk`.  For example, replace

`https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-06-09`

with `https://github.com/rfordatascience/tidytuesday/trunk/data/2020/2020-06-09`

3.  Download the folder using the terminal window in RStudio.

`svn checkout https://github.com/rfordatascience/tidytuesday/trunk/data/2020/2020-06-09`

4. Create a new gh-pages branch.  Build the Rmarkdown file (and resulting HTML) for analyzing the data.






