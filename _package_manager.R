# source "https://gist.github.com/smithdanielle/9913897"
# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# create settings 
if(exists("Settings") != TRUE) {
  # create Settings list
  Settings <- list()
}

# set packages to install
Settings$packages<-c("ggplot2", 
            "tidyr", 
            "readr",
            "dplyr",
            "jsonlite",
            "trelloR",
            "purrr",
            "httpuv",
            "httr",
            "stringr",
            "reshape2",
            "lubridate",
            "magrittr",
            "grid",
            "gridExtra",
            "pryr",
            "devtools",
            "plotly",
            "scales")
check.packages(Settings$packages)
