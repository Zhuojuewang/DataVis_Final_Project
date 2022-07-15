library(rworldmap)
library(highcharter)
library(dplyr)
library(maps)
library(tidyverse)
library(leaflet)
library(rgdal)

d <- read.csv("Crypoto/Data for legalization_encoded.csv")
dat <- iso3166
dat <- rename(dat, "iso-a3" = a3)
dat_joined <- dat %>% left_join(d,by=c("sovereignty"="Country"))
dta_clss <- list("Banned","Legally restricted","Not Clear","Legal Tender","Legal")

hcmap(
  map = JS("https://code.highcharts.com/mapdata/custom/world-highres3.js"), #"custom/world-highres3", # high resolution world map
  data = dat_joined, # name of dataset
  joinBy = "iso-a3",
  value = "State",
  showInLegend = "False", # hide legend
  nullColor = "#DADADA",
  download_map_data = TRUE,
  name = "Legality Situation",
  tooltip = list(pointFormat = "{point.name} :{point.State_True}"),
  dataLabels = list(enabled = TRUE, format = "{State_True}")
) %>%
  hc_colorAxis(stops = color_stops(),label=c("Banned","Legally restricted","Not Clear","Legal Tender","Legal")) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_legend("False") %>%
  hc_title(text = "World Map") %>%
  hc_title(text = "Cryto Legality Situation In Main Countries of the World") 


