
# Linking demographics and geography: Using gridded population to gain spatial insights in R

## Introduction

Access to **high-resolution population counts** is key for local, national and international decision-making and intervention. It supports data-driven planning of critical infrastructures, such as schools, health facilities and transportation networks. 

**WorldPop** has developed modelling techniques to estimate population in grid cells of 100m by 100m by disaggregating census-based population totals for the entire world, leveraging the growing availability of products derived from satellite imagery. This level of detail offers the advantage of flexible aggregation of the population estimates within different administrative and functional units, for instance, school catchment areas and health zones. 

This session will cover the notion of **gridded population**, a data format at the crossroad of demography and geography. We will then have a brief overview of openly available **satellite-imagery-based products** that can be used for modelling gridding population and beyond, such as settlement maps. Finally, we will have some hands-on to extract information from a gridded population covering the following R packages for geospatial analysis: `sf` [(Pebesma, E., 2018)](https://r-spatial.github.io/sf/), `terra` [(Hijmans, R., 2021)](https://rspatial.github.io/terra/reference/terra-package.html), and `tmap` [(Tennekes, M., 2018)](https://r-tmap.github.io/tmap/index.html).

## Challenge

We will study the question: *How many women of childbearing age are struggling to access maternal health services?*

![map](/pic/final_map.png)


## Concepts

This tutorial covers the concepts of:
- interactive mapping
- vector file reading and filtering
- raster file reading
- spatial projections
- buffering
- rasterising
- zonal statistics
- masking

## Contents

The script [`exercises.R`](exercises.R) contains the workflow with the questions.
The script [`solutions.R`](solutions.R) contains the workflow with the answers.
The powerpoint [`SICSS_202309_griddedPop.pptx`](SICSS_202309_griddedPop.pptx) contains the presentation.

## Data used

For that purpose, we will need to access three data sources:
1. Population data from the Bottom-up gridded population estimates for Nigeria, version 2.0, produced jointly by WorldPop and the National Population Commission of Nigeria and  accessible [here](https://wopr.worldpop.org/?NGA/Population/v2.0),
2. Health facilities locations produced by GRID3 Nigeria and accessible [here](https://data.grid3.org/datasets/GRID3::grid3-nigeria-health-care-facilities--1/about),
3. Local Government Area operational boundaries released by GRID3 Nigeria and accessible [here](https://data.grid3.org/datasets/GRID3::grid3-nigeria-local-government-area-boundaries/about)

## Suggested citation
Darin E, Tatem AJ. 2023. Linking demographics and geography: Using gridded population to gain spatial insights in R. Summer Institute of Computational Social Sciences, University of Calabar, Nigeria. https://github.com/wpgp/SICSS-calabar-gridded-population. 

![sicss](/pic/sicss.png) ![worldpop](/pic/worldpop.png)