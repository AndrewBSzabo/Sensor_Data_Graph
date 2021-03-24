library(leaflet)
library(readr)
library(readxl)
library(tigris)
library(dplyr)
library(tibble)
library(DT)
library(tidygeocoder)
library(sf)
library(raster)

# ----------- COVID DATA BY ZIP -----------
COVID_DATA_PATH <- "Covid Data/Date 03_22_2021.csv"
covid_data_by_zip_oh <- read_csv(COVID_DATA_PATH, 
                                 col_types = cols(`Zip Code` = col_character(), 
                                                  Population = col_number(), `Case Count - Cumulative` = col_number(), 
                                                  `Case Count - Last 30 Days` = col_number(), 
                                                  `Case Count - Last 14 Days` = col_number(), 
                                                  `Case Count Per 100K - Cumulative` = col_number(), 
                                                  `Case Count Per 100K - Last 30 Days` = col_number(), 
                                                  `Case Count Per 100K - Last 14 Days` = col_number()))

colnames(covid_data_by_zip_oh) <- c("y_n","zip","pop","cc_cum","cc_30","cc_14","cc_100_cum","cc_100_30","cc_100_14")

covid_data_by_zip_greater_cle <- covid_data_by_zip_oh %>% dplyr::select(zip,cc_100_cum) #filter(covid_data_by_zip_oh, y_n == "Y") %>%
  

# starts_with=44 to simplify dataset to zips around cleveland
zip_geo_oh <- zctas(cb = TRUE, state='OH', year=2019, starts_with="44")

colnames(zip_geo_oh) <- c("zip","aff_geo_id","geo_id","a_land","a_water","geometry")

covid_and_geo <- geo_join(zip_geo_oh,covid_data_by_zip_greater_cle,'zip','zip', how="inner")

# zipcodes and shapes WORKING ON THIS!!!!!!!!!
#zip_shape <- select(covid_and_geo,zip,geometry)
#write.csv(zip_shape,"data sets\\zip_and_shapes.csv")
#shapefile(zip_shape, "data sets/zip_and_shapes.shp")
# ----------------------

# ----------- CLEVELAND MONITOR LOCATION DATA -----------
CLE_VALUES = c("Does not Measure PM2.5","Measures PM2.5 (Continuous)")
CLE_VALUES_COLORS = c("black","blue")

CDPH_AQ_monitor_locations <- read_excel("Monitor Data/CDPH-AQ Monitor Locations.xlsx", 
                                        skip = 2)

colnames(CDPH_AQ_monitor_locations) <- c("name","addresses", "pm_10","air_toxic","metals","pm_2_5","pm_2_5_s","s_02","O_zone","CO","Nox","PM")

cle_addresses <- tibble(singlelineaddress = CDPH_AQ_monitor_locations$addresses)
cle_address_and_latlong <- cle_addresses %>% geocode(address = singlelineaddress, method = 'cascade', verbose=TRUE)

CDPH_AQ_monitor_locations$lat <- cle_address_and_latlong$lat
CDPH_AQ_monitor_locations$long <- cle_address_and_latlong$long
# ----------------------

# ----------- OUR MONITOR LOCATION DATA -----------
# IOTC
IOTC <- read_csv("Monitor Data/Sensor Deployment - Potential Sites - 011421 v2 - Sheet1.csv", 
                 skip = 1)
colnames(IOTC) <- c("num","name","zip","addresses","y_n","links")
IOTC$addresses <- paste0(IOTC$addresses, ", ", IOTC$zip)

IOTC <- IOTC %>% filter(y_n == TRUE)

IOTC_addresses <- tibble(singlelineaddress = IOTC$addresses)
IOTC_address_and_latlong <- IOTC_addresses %>% geocode(address = singlelineaddress, method = 'cascade', verbose=TRUE)

IOTC$lat <- IOTC_address_and_latlong$lat
IOTC$long <- IOTC_address_and_latlong$long

# CCPL
CCPL <- read_excel("Monitor Data/Sensor Deployment - CCPL Confirmed Sites - 032421.xlsx", 
                   skip = 1,
                   n_max = 29)
colnames(CCPL) <- c("num","name","selected","y_n","comments","zip","addresses","random")
CCPL$addresses <- paste0(CCPL$addresses, ", ", CCPL$zip)

CCPL <- CCPL %>% filter(y_n != 'N')

CCPL_addresses <- tibble(singlelineaddress = CCPL$addresses)
CCPL_addresses_and_latlong <- CCPL_addresses %>% geocode(address = singlelineaddress, method = 'cascade', verbose=TRUE)

CCPL$lat <- CCPL_addresses_and_latlong$lat
CCPL$long <- CCPL_addresses_and_latlong$long

# CPL
CPL <- read_excel("Monitor Data/Sensor Deployment - CPL Confirmed Sites - 031521.xlsx", 
                  skip = 1)
colnames(CPL) <- c("num","name","y_n","proposed","comments","zip","addresses","random")
CPL$addresses <- paste0(CPL$addresses, ", ", CPL$zip)

CPL <- CPL %>% filter(y_n != 'N')

CPL_addresses <- tibble(singlelineaddress = CPL$addresses)
CPL_addresses_and_latlong <- CPL_addresses %>% geocode(address = singlelineaddress, method = 'cascade', verbose=TRUE)

CPL$lat <- CPL_addresses_and_latlong$lat
CPL$long <- CPL_addresses_and_latlong$long
# ----------------------

# ---------------------------------------
# ----------- GRAPH VARIABLES -----------
# ---------------------------------------

GRAPH_LAYERS = c("Covid Data","CDPH-DAQ Monitoring (triangles)","IOTC (circles)", "CPL (stars)", "CCPL (squares)")

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
            title = paste("Covid Case Per 100K "," (03/22/2021):"),
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
              iconUrl = ifelse(!is.na(CDPH_AQ_monitor_locations$PM),
                               'icons/triangle-blue.png',
                               'icons/triangle-black.png'
              ),
              iconAnchorX = 5, iconAnchorY = 5,
              iconWidth = 10, iconHeight = 10
            ),
            group = GRAPH_LAYERS[2]) %>%
  
  # Our Sensors
  # IOTC
  addMarkers(lng=IOTC$long, 
             lat=IOTC$lat,
             popup = paste(as.character(IOTC$name), ", ", as.character(IOTC$addresses)), 
             icon= icons(
               iconUrl = 'icons/circle-yellow.png',
               iconAnchorX = 4, iconAnchorY = 4,
               iconWidth = 8, iconHeight = 8
             ),
             group = GRAPH_LAYERS[3]) %>%
  # CPL
  addMarkers(lng=CPL$long, 
             lat=CPL$lat,
             popup = paste(as.character(CPL$name), ", ", as.character(CPL$addresses)), 
             icon= icons(
               iconUrl = 'icons/star-yellow.png',
               iconAnchorX = 5, iconAnchorY = 5,
               iconWidth = 10, iconHeight = 10
             ),
             group = GRAPH_LAYERS[4]) %>%
  
  # CCPL
  addMarkers(lng=CCPL$long, 
             lat=CCPL$lat,
             popup = paste(as.character(CCPL$name), ", ", as.character(CCPL$addresses)), 
             icon= icons(
               iconUrl = 'icons/square-yellow.png',
               iconAnchorX = 4, iconAnchorY = 4,
               iconWidth = 8, iconHeight = 8
             ),
             group = GRAPH_LAYERS[5]) %>%
  
  # Scale
  addScaleBar(position = "topleft") %>%
  
  # Layer Control
  addLayersControl(
    position = "bottomright",
    overlayGroups = GRAPH_LAYERS,
    options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))

