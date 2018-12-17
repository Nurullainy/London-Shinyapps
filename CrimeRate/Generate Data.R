library(tmap)
library(tmaptools)
library(leaflet)
library(sf)
library(leaflet.extras)
library(dplyr)
library(tidyverse)

# General Data Generation--------------------------------------------------------------------------------------
crimefile = read.csv("Final Crime Data.csv")
crimetype = read.csv("Final Crime Types.csv")

crime2012 = read.csv("2012 Crime Data.csv")
crime2013 = read.csv("2013 Crime Data.csv")
crime2014 = read.csv("2014 Crime Data.csv")
crime2015 = read.csv("2015 Crime Data.csv")
crime2016 = read.csv("2016 Crime Data.csv")
ukcode = levels(crime2014$LAD11CD)
ukshapefile <- "UK Shapefile/Local_Authority_Districts_December_2016_Generalised_Clipped_Boundaries_in_the_UK_WGS84.shp"

# Read in the shapefile for US states and counties:
GBgeo <- read_shape(file=ukshapefile, as.sf = TRUE)

# Subset just the London data from the GB shapefile
ukgeo <- dplyr::filter(GBgeo, lad16cd %in% ukcode)
ukgeo <- sf::st_transform(ukgeo, "+proj=longlat +datum=WGS84")

# The factor for lad16nm needs to be equal to 33, so:
ukgeo$lad16nm = factor(ukgeo$lad16nm)


# Change the city names to plain characters in ukgeo
ukgeo$lad16nm <- as.character(ukgeo$lad16nm)

# Order each data set by county name
ukgeo <- ukgeo[order(ukgeo$lad16nm),]
ukgeo <- rename(ukgeo, City = lad16nm)

# Data Generation for 2012-------------------------------------------------------------------------------------

# Change and order the city names to plain characters in crime data:
crime2012$City <- as.character(crime2012$City)
crime2012 <- crime2014[order(crime2012$City),]

# Merge data with tmaptool's append_data function
ukmap2012 <- append_data(ukgeo, crime2012, key.shp = "City", key.data="City")


# Data Generation for 2013-------------------------------------------------------------------------------------

# Change and order the city names to plain characters in crime data:
crime2013$City <- as.character(crime2013$City)
crime2013 <- crime2013[order(crime2013$City),]

# Merge data with tmaptool's append_data function
ukmap2013 <- append_data(ukgeo, crime2013, key.shp = "City", key.data="City")


# Data Generation for 2014-------------------------------------------------------------------------------------

# Change and order the city names to plain characters in crime data:
crime2014$City <- as.character(crime2014$City)
crime2014 <- crime2014[order(crime2014$City),]

# Merge data with tmaptool's append_data function
ukmap2014 <- append_data(ukgeo, crime2014, key.shp = "City", key.data="City")


# Data Generation for 2015-------------------------------------------------------------------------------------

# Change and order the city names to plain characters in crime data:
crime2015$City <- as.character(crime2015$City)
crime2015 <- crime2015[order(crime2015$City),]

# Merge data with tmaptool's append_data function
ukmap2015 <- append_data(ukgeo, crime2015, key.shp = "City", key.data="City")


# Data Generation for 2016-------------------------------------------------------------------------------------

# Change and order the city names to plain characters in crime data:
crime2016$City <- as.character(crime2016$City)
crime2016 <- crime2014[order(crime2016$City),]

# Merge data with tmaptool's append_data function
ukmap2016 <- append_data(ukgeo, crime2016, key.shp = "City", key.data="City")

