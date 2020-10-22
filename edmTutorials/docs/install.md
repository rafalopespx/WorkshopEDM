# Installation instructions

These tutorials cover the use of an [R](https://www.r-project.org/) package by Sugihara Lab (UCSD/SIO) and collaborators called [rEDM](https://cran.r-project.org/web/packages/rEDM/index.html). We recommend the use of [RStudio](https://www.rstudio.com/), which makes it easier to code and run R programs and notebooks in Rmd format. You can skip the download of RStudio if you already use R or are familiar with the use of a command-line interface.

## Installation steps

All software below is available for Windows, Mac and Linux. For the latter, many distributions provide their own packages, which can be easier than downloading and installing manually.

- [download](https://cran.rstudio.com/) and install R
- [download](https://www.rstudio.com/products/rstudio/download/) and install RStudio
- install the packages `rEDM` and `dplyr` issuing the following command (or using the Tools > Install packages option from the RStudio dropdown menu):
```R
    install.packages(c('rEDM', 'dplyr'))
```
- to run the interactive plots in your own computer, you also need the `plotly` package (`install.packages(c('plotly'))`) - this is not needed because the pages already allow exploration from your browser, and this package has more library dependencies, so it is more trouble to install.

Now you can download and run the tutorials step-by-step, and adapt them to your own needs.

