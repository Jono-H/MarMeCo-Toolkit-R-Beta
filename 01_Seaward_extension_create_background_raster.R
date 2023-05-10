---
title: "01_Seaward_extensions_create_background_raster"
author: "Bethany Clark"
date: '2023-03-27'
output: html_document
---

<!--- This is an HTML comment in RMarkdown. You can use these comments to make notes that won't get read when running the code -->

<!--- {-} curly brackets hash means subsections won't get numbered -->

# Seaward Extension Guidance {.unnumbered}

This tutorial uses the example data for Adelie Penguins. For information on the original study, see Handley et al. 2021 Marine Important Bird and Biodiversity Areas for Penguins in Antarctica, Targets for Conservation Action. Frontiers in Marine Science 7: 602972. https://doi.org/10.3389/fmars.2020.602972

The tutorial aims to further explain what the functions are doing and acts as a guide for inexperienced users. Users should consult this tutorial in conjunction with the 2021 manuscript, supplementary material and GitHub account.

While track2KBA is designed to allow minimal input from the user, users should understand the consequence of choices to input parameters while using the functions provided within track2KBA.

<br> <br>

**What does this tutorial cover:**

-   Overview of the Track2KBA R package\

-   Summary of the Track2KBA scientific paper

    -   <https://doi.org/10.1111/2041-210X.13713>\

-   Incorporating further details from the Track2KBA paper supplementary material\

-   Incorporating details from the supporting Track2KBA GitHub tutorial

    -   <https://github.com/BirdLifeInternational/track2kba>

<!--- Add a space after the line to start the bulleted list -->

<!--- Placing **text** will make text bold -->

**Output(s) from this script, can be used to:**

-   assess data against relevant criteria, such as:

    -   IBA criteria: <http://datazone.birdlife.org/site/ibacriteria>

    -   KBA criteria: <https://www.keybiodiversityareas.org/working-with-kbas/proposing-updating/criteria>\

-   or, for identification of sites from animal tracking data to be used in alternate spatial planning exercises.

This analysis was performed in **`r sessionInfo()$R.version$version.string`**\

This document was last updated on **`r Sys.Date()`** <br>

<!--- In the code chunk below, we specify include = F, so that we will run the chunk but not include the chunk in the final document. We set a global argument in the code chunk of echo = T, so that in later code chunks, the code will be displayed in the RMarkdown document -->

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```

## Load packages

**Load required R packages:**

If the packages fail to load, you will need to install the relevant packages.

```{r load-packages, include = TRUE}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load libraries --------------------------------------------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(track2KBA)
library(tidyverse)
library(readxl)
library(sf)
library(xlsx)
library(sp)
library(gridExtra)
library(viridis)
```

