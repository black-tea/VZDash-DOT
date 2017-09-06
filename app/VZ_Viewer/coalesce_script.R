##### Setup
library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) 
library(sp)
library(rgeos)
library(sf)
library(mapview)
library(webshot)
library(htmlwidgets)
library(units)

# Set WD
work_dir <- "C:/Users/dotcid034/Documents/GitHub/vzcd-shiny/app/VZ_Viewer"
setwd(work_dir)

lapd_collisions <- rgdal::readOGR('data/shp/lapd_collisions')
lapd_collisions <- st_as_sf(lapd_collisions)


# This formula (below) replaces the 'Y' factor with the 'bike' factor
levels(lapd_collisions$bike_inv)[match('Y',levels(lapd_collisions$bike_inv))] <- "bike"
levels(lapd_collisions$bike_inv) <- c(levels(lapd_collisions$bike_inv),'ped')
# This formula (below) replaces the 'Y' factor with the 'ped' factor
levels(lapd_collisions$ped_inv)[match('Y',levels(lapd_collisions$ped_inv))] <- "ped"
levels(lapd_collisions$ped_inv) <- c(levels(lapd_collisions$ped_inv),'bike')


# Ultimately, what I need to do is 'coalesce' the two columns into one
lapd_collisions$mode <- coalesce(lapd_collisions$ped_inv, lapd_collisions$bike_inv)

data(tli)
library(xtable)
xtable(tli[1:10,])