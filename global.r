library(shiny)
library(shinyjqui)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(sf)
library(readr)
library(RColorBrewer)
library(DT)



#Cleaned data
joineddf3_sf <- readRDS("./DemoTrends/joineddf3_sf.rds")
joineddf2 <- readRDS("./DemoTrends/joineddf2.rds")

