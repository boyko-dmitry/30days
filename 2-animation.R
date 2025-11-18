# The goal is to make a video of vehicles moving along their
# routes of different colours using GTFS data

#start----
# import libraries
library(tidyverse)
library(tidytransit)
library(sf)
library(leaflet)
library(gtfsrouter)
library(ggspatial)
library(rosm)
library(osmextract)
library(rnaturalearth)

# import GTFS
gtfs_path <- "~/30days/gtfs.zip"
gtfs <- read_gtfs(gtfs_path)

# 1. filter lines to animate----
gtfs_full <- gtfs
gtfs$routes <- gtfs$routes %>%
  filter(agency_id == 16)
