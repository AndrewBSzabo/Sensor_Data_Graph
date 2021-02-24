library(leaflet)
library(readr)
library(readxl)
library(tigris)
library(dplyr)
library(tibble)
library(DT)
library(tidygeocoder)

# ----------- COVID DATA BY ZIP -----------
COVID_DATA_PATH <- "Covid Data/Date 02_22_2021.csv"
covid_data_by_zip_oh <- read_csv(COVID_DATA_PATH, 
                                 col_types = cols(`Zip Code` = col_character(), 
                                                  Population = col_number(), `Case Count - Cumulative` = col_number(), 
                                                  `Case Count - Last 30 Days` = col_number(), 
                                                  `Case Count - Last 14 Days` = col_number(), 
                                                  `Case Count Per 100K - Cumulative` = col_number(), 
                                                  `Case Count Per 100K - Last 30 Days` = col_number(), 
                                                  `Case Count Per 100K - Last 14 Days` = col_number()))

colnames(covid_data_by_zip_oh) <- c("y_n","zip","pop","cc_cum","cc_30","cc_14","cc_100_cum","cc_100_30","cc_100_14")

covid_data_by_zip_greater_cle <- select(covid_data_by_zip_oh,zip,cc_100_cum) #filter(covid_data_by_zip_oh, y_n == "Y") %>%
  

# starts_with=44 to simplify dataset to zips around cleveland
zip_geo_oh <- zctas(cb = TRUE, state='OH', year=2019, starts_with="44")

colnames(zip_geo_oh) <- c("zip","aff_geo_id","geo_id","a_land","a_water","geometry")

covid_and_geo <- geo_join(zip_geo_oh,covid_data_by_zip_greater_cle,'zip','zip', how="inner")
# ----------------------

# ----------- CLEVELAND MONITOR LOCATION DATA -----------
CLE_VALUES = c("Does not Measure PM2.5","Measures PM2.5")
CLE_VALUES_COLORS = c("black","blue")

CDPH_AQ_monitor_locations <- read_excel("CDPH-AQ Monitor Locations.xlsx", 
                                        skip = 2)

colnames(CDPH_AQ_monitor_locations) <- c("name","addresses", "pm_10","air_toxic","metals","pm_2_5","pm_2_5_s","s_02","O_zone","CO","Nox","PM")

cle_addresses <- tibble(singlelineaddress = CDPH_AQ_monitor_locations$addresses)
cle_address_and_latlong <- cle_addresses %>% geocode(address = singlelineaddress, method = 'cascade', verbose=TRUE)

CDPH_AQ_monitor_locations$lat <- cle_address_and_latlong$lat
CDPH_AQ_monitor_locations$long <- cle_address_and_latlong$long
# ----------------------

# ----------- OUR MONITOR LOCATION DATA (POSSIBLE) -----------
POTENTIAL = c("County Library", "Cleveland Public Library")
POTENTIAL_COLORS = c("darkcyan","cyan")

sensor_deployment_potential_sites <- read_csv("Sensor Deployment - Potential Sites - 011421 v2 - Sheet1.csv", 
                                                               skip = 1)
colnames(sensor_deployment_potential_sites) <- c("num","name","zip","addresses","selected","links")
sensor_deployment_potential_sites$addresses <- paste(sensor_deployment_potential_sites$addresses, ", ", sensor_deployment_potential_sites$zip)

our_addresses <- tibble(singlelineaddress = sensor_deployment_potential_sites$addresses)
our_address_and_latlong <- our_addresses %>% geocode(address = singlelineaddress, method = 'cascade', verbose=TRUE)


color_vec = vector("character", dim(sensor_deployment_potential_sites)[1])
  
for(i in 1:dim(sensor_deployment_potential_sites)[1]) {
  for(j in 1:length(POTENTIAL)) {
    if (sensor_deployment_potential_sites[i,]$selected == TRUE) {
      color_vec[i] = 'yellow'
    }
    else if (grepl(POTENTIAL[j],sensor_deployment_potential_sites[i,]$name, fixed=TRUE)) {
      color_vec[i] = POTENTIAL_COLORS[j]
    }
  }
}

sensor_deployment_potential_sites$color <- color_vec
sensor_deployment_potential_sites$lat <- our_address_and_latlong$lat
sensor_deployment_potential_sites$long <- our_address_and_latlong$long

IOTC_sensors <- subset(sensor_deployment_potential_sites,color=='yellow')
potential_CCPL <- subset(sensor_deployment_potential_sites,color==POTENTIAL_COLORS[1])
potential_CPL <- subset(sensor_deployment_potential_sites,color==POTENTIAL_COLORS[2])
# ----------------------

# ---------------------------------------
# ----------- GRAPH VARIABLES -----------
# ---------------------------------------

GRAPH_LAYERS = c("Covid Data","CDPH-DAQ Monitoring","IOTC Sensors", "Potential CPL", "Potential CCPL")

covid_pal <- colorNumeric(palette = "Reds", domain = covid_and_geo$cc_100_cum)
cle_monitor_pal <- colorFactor(CLE_VALUES_COLORS, CLE_VALUES, ordered=T)

popup <- paste0("zip: ", as.character(covid_and_geo$zip), "; value: ", as.character(covid_and_geo$cc_100_cum))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Covid Data
  addPolygons(data = covid_and_geo, 
              fillColor = ~covid_pal(covid_and_geo$cc_100_cum), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup,
              group = GRAPH_LAYERS[1]) %>%
  addLegend(pal = covid_pal, 
            values = covid_and_geo$cc_100_cum, 
            position = "topright", 
            title = paste("Covid Case Per 100K "," (02/22/2021):"),
            group = GRAPH_LAYERS[1]) %>%
  
  # Cleveland Sensors
  addLegend(pal=cle_monitor_pal,
            values = CLE_VALUES,
            position = "bottomleft",
            title = "CDPH-DAQ Monitor Type:",
            group = GRAPH_LAYERS[2]) %>%
  addMarkers(lng=CDPH_AQ_monitor_locations$long, 
            lat=CDPH_AQ_monitor_locations$lat,
            popup = paste(as.character(CDPH_AQ_monitor_locations$name), ", ", as.character(CDPH_AQ_monitor_locations$addresses)), 
            icon= icons(
              iconUrl = ifelse(!is.na(CDPH_AQ_monitor_locations$pm_2_5) | !is.na(CDPH_AQ_monitor_locations$pm_2_5_s),
                               'triangle-blue.png',
                               'triangle-black.png'
              ),
              iconAnchorX = 5, iconAnchorY = 5,
              iconWidth = 10, iconHeight = 10
            ),
            group = GRAPH_LAYERS[2]) %>%
  
  # Our Potential Sensors
  # IOTC Sensors
  addCircleMarkers(lng=IOTC_sensors$long, 
                   lat=IOTC_sensors$lat, 
                   popup = paste(as.character(IOTC_sensors$name), ", ", as.character(IOTC_sensors$addresses)), 
                   radius = 0.5, 
                   color = IOTC_sensors$color, 
                   opacity=0.9,
                   group = GRAPH_LAYERS[3]) %>%
  # Potential CPL
  addCircleMarkers(lng=potential_CPL$long, 
                   lat=potential_CPL$lat, 
                   popup = paste(as.character(potential_CPL$name), ", ", as.character(potential_CPL$addresses)), 
                   radius = 0.5, 
                   color = potential_CPL$color, 
                   opacity=0.9,
                   group = GRAPH_LAYERS[4]) %>%
  
  # Potential CCPL
  addCircleMarkers(lng=potential_CCPL$long, 
                   lat=potential_CCPL$lat, 
                   popup = paste(as.character(potential_CCPL$name), ", ", as.character(potential_CCPL$addresses)), 
                   radius = 0.5, 
                   color = potential_CCPL$color, 
                   opacity=0.9,
                   group = GRAPH_LAYERS[5]) %>%
  
  # Scale
  addScaleBar(position = "topleft") %>%
  
  # Layer Control
  addLayersControl(
    position = "bottomright",
    overlayGroups = GRAPH_LAYERS,
    options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))

