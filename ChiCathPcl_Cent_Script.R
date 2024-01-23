### Zoning and Houses of Worship - Chicago - Public data
## Madeline Johnson, Notre Dame Church Properties Initiative
## Jan 2024

# The purpose of this script is to derive summary stats about church property
# in Chicago to support Fr. Pat Reidy's Yale Law Journal article about
# zoning, houses of worship, affordable housing, and religious land use.

# The analysis draws on exclusively public data supplied the the City of Chicago
# (zoning) and Cook County (tax exempt parcel data).

### Initialize

## Load packages

install.packages("ggthemes")

library(dplyr)
library(sf)
library(ggplot2)
library(tmap)
library(tidyverse)
library(leaflet)
library(mapview)
library(shiny)
library(readxl)
library(raster)
library(xml2)
library(tidygeocoder)
library(magrittr)
library(units)
library(ggthemes)

sf_use_s2(FALSE)
mapviewOptions(fgb = FALSE)

## Set working directory

setwd("~/Desktop/Lab/ChicagoPublic")

## Identify parcels held in name of Bishop, join to parcel shapefiles, and map

## Load data
CookCty_Pcl <- st_read("CookCty_Pcl/CookCty_Pcl.shp")
Chi_Bnd <- st_read("Chi_Bnd.shp")
CookCty_Exempt <- read_excel("CookCty_ExemptPcls.xlsx", sheet = 1)

# filter exempt file to tax year 2023

CookCty_Exempt <- CookCty_Exempt %>% filter(tax_year == "2023")

## Filter exempt file to Catholic Bishop
CathPclList <- CookCty_Exempt %>% filter(owner_name == "CATH CH ARCHDIOC CHGO"
                                         | owner_name == "CATHOLIC BISHOP CHGO")



## Join list to parcel shapefile

CookCathPcls <- inner_join(CookCty_Pcl, CathPclList, by = 'pin10')

# check for dupes

CookDupesPins <- data.frame(duplicated(CookCty_Pcl$pin10))
CookDupesGeoms <- data.frame(duplicated(CookCty_Pcl$geometry))
# lots of dupes... but I think in the join it just takes the first one so maybe it's okay?

# calculate area before creating centroid for plotting

CookCathPcls$area <- st_area(CookCathPcls)
# determined that units are SF; create SF column and remove units which can be problematic

CookCathPcls$SF <- CookCathPcls$area

CookCathPcl_Cent <- st_centroid(CookCathPcls)

# filter to Chicago boundaries

ChiCathPcl_Cent <- st_filter(CookCathPcl_Cent, Chi_Bnd)

## here is where I get the issue -- reproject Chi_Bnd

st_crs(CookCathPcl_Cent)
# crs = 3435

Chi_Bnd_reproj <- st_transform(Chi_Bnd, 3435)

ChiCathPcl_Cent <- st_filter(CookCathPcl_Cent, Chi_Bnd_reproj)

mapview(ChiCathPcl_Cent)

# export to shapefile

ChiCathPcl_Cent_new <- st_write(ChiCathPcl_Cent, "ChiCathPcl_Cent_new.shp")

# test print map

propertymap <- ggplot() +
  theme_bw() +
  geom_sf(data = ChiCathPcl_Cent_new, aes(size = "SF"), shape = 16, color = "black",
          alpha = .9) +
  theme_map() +
  coord_sf()

propertymap

# projection looks okay but getting warning message
# "Warning message:
# Using size for a discrete variable is not advised."

# drop_units for SF column above?
# CookCathPcls$SF <- drop_units(CookCathPcls$area)
#  doesn't fix it