## Script to execute R Markdown functions

## load libraries
library(rmarkdown)
library(knitr)

## check working directory
getwd()

## knit the specific document of interest
rmarkdown::render("./R-RMarkdown-BookdownChapters/02-TrackingData-SamplingStrategy.Rmd",
                  output_format = "html_document",
                  output_dir = "R-RMarkdown-BookdownChapters/html-test-files")

