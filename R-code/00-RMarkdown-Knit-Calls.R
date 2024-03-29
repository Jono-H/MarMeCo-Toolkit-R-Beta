## Script to execute R Markdown functions

## load libraries
library(rmarkdown)
library(knitr)

## check working directory
getwd()

## knit the specific document of interest
rmarkdown::render("./R-RMarkdown-BookdownChapters/03-TrackingData-Visualisation.Rmd",
                  output_format = "html_document",
                  output_dir = "R-RMarkdown-BookdownChapters/html-test-files")

## Create r script from markdown document
knitr::purl(input = "./R-RMarkdown-BookdownChapters/08-TrackingData-YelkouanExample.Rmd", 
            output = "./R-RMarkdown-BookdownChapters/08-TrackingData-YelkouanExample-Rscript.R", 
            documentation = 2)
