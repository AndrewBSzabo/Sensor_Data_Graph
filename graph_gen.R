library(leaflet)
library(readr)
library(readxl)
library(tigris)
library(dplyr)
library(tibble)
library(DT)
library(tidygeocoder)

# ----------- COVID DATA BY ZIP -----------
covid_data_by_zip_oh <- read_csv("OH COVID Zip Codes 113020 v2 - COVIDSummaryDataZIP.csv", 
                                                             col_types = cols(`Zip Code` = col_character(), 
                                                                              Population = col_number(), `Case Count - Cumulative` = col_number(), 
                                                                              `Case Count - Last 30 Days` = col_number(), 
                                                                              `Case Count - Last 14 Days` = col_number(), 
                                                                              `Case Count Per 100K - Cumulative` = col_number(), 
                                                                              `Case Count Per 100K - Last 30 Days` = col_number(), 
                                                                              `Case Count Per 100K - Last 14 Days` = col_number()))

colnames(covid_data_by_zip_oh) <- c("y_n","zip","pop","cc_cum","cc_30","cc_14","cc_100_cum","cc_100_30","cc_100_14", "city")

covid_data_by_zip_greater_cle <- filter(covid_data_by_zip_oh, y_n == "Y") %>%
  select(zip,cc_100_cum)

# starts_with=44 to simplify dataset to zips around cleveland
zip_geo_oh <- zctas(cb = TRUE, state='OH', year=2019, starts_with="44")

colnames(zip_geo_oh) <- c("zip","aff_geo_id","geo_id","a_land","a_water","geometry")

covid_and_geo <- geo_join(zip_geo_oh,covid_data_by_zip_greater_cle,'zip','zip', how="inner")
# ----------------------

# ----------- MONITOR LOCATION DATA -----------
CDPH_AQ_monitor_locations <- read_excel("CDPH-AQ Monitor Locations.xlsx", 
                                        skip = 2)

colnames(CDPH_AQ_monitor_locations) <- c("name","addresses", "pm_10","air_toxic","metals","pm_2_5","pm_2_5_s","s_02","O_zone","CO","Nox","PM")

addresses <- tibble(singlelineaddress = CDPH_AQ_monitor_locations$addresses)
address_and_latlong <- addresses %>% geocode(address = singlelineaddress, method = 'cascade', verbose=TRUE)

CDPH_AQ_monitor_locations$lat <- address_and_latlong$lat
CDPH_AQ_monitor_locations$long <- address_and_latlong$long
# ----------------------

GRAPH_LAYERS = c("Covid Data","Cleveland Sensors","Our Sensors")

covid_pal <- colorNumeric("Reds", covid_and_geo$cc_100_cum)
monitor_pal <- colorFactor(c("Black","Blue"), c("Does not Measure PM2.5","Measures PM2.5"))

popup <- paste0("zip: ", as.character(covid_and_geo$zip), "; value: ", as.character(covid_and_geo$cc_100_cum))

leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Covid Data
  addPolygons(data = covid_and_geo, 
              fillColor = ~pal(covid_and_geo$cc_100_cum), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup,
              group = GRAPH_LAYERS[1]) %>%
  addLegend(pal = covid_pal, 
            values = covid_and_geo$cc_100_cum, 
            position = "topleft", 
            title = "Covid Case Per 100K - Cumulative:",
            group = GRAPH_LAYERS[1]) %>%
  
  # Cleveland Sensors
  addLegend(pal=monitor_pal,
            values = c("Does not Measure PM2.5","Measures PM2.5"),
            position = "bottomleft",
            title = "Monitor Type:",
            group = GRAPH_LAYERS[2]) %>%
  addCircleMarkers(lng=CDPH_AQ_monitor_locations$long, 
                   lat=CDPH_AQ_monitor_locations$lat, 
                   popup = as.character(CDPH_AQ_monitor_locations$name), 
                   radius = 1, 
                   color = ifelse(!is.na(CDPH_AQ_monitor_locations$pm_2_5) | !is.na(CDPH_AQ_monitor_locations$pm_2_5_s),'blue','black'), 
                   opacity=0.9,
                   group = GRAPH_LAYERS[2]) %>%
  
  # Scale
  addScaleBar(position = "topright") %>%
  
  # Layer Control
  addLayersControl(
    position = "bottomright",
    overlayGroups = GRAPH_LAYERS,
    options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))

