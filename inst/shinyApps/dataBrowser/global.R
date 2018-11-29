pb = winProgressBar(
  title = sprintf('Starting %s ...', "dataBrowser"),
  label = 'Initializing ...'
)
setWinProgressBar(pb, 1.00, label = 'Starting application')

library(ggplot2)
library(DBI)
library(RMariaDB)
library(dplyr)
#library(dbplyr)
library(lubridate)
library(plotly)
library(shinydashboard)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr)
library(rgdal)
library(shinycssloaders)
library(tidyr)
library(SBR)



Logged = FALSE

Host <- "95.171.35.104"

wd<-getwd()
file<-file.path(wd,"Orchards_monitoring_station_MONALISA-SBR_Project_21032018.kml")

# y axis title
RHy <- list(title = "Air RH [%]")
Airy <- list(title = "Air & Soil T [°C]")
Py <- list(title = "P & Irrig [mm]")
windy <- list(title = "Wind speed [m/s]")
SWCy <- list(title = "SWC [%]")
SWPy<- list(title = "SWP [kPa]")
STy <- list(title = "Soil T [°C]")

# colors
RHcolor<-"#0188AE"
AirTmeancolor<-"#000A10"
AirTmaxncolor<-"#D81159"
AirTmincolor<-"#0496FF"
Pcolor<-"#1778AE"
Irrigcolor<-"#00273D"
Windcolor<-"#0D0508"
SWC20color<-"#8F2D56"##same for SWP'red'
SWC40color<-"#006BA6"#'blue'#same for SWP
ST20color<-'#FFBC42'#"#8F2D56"'red'
ST40color<-'#74561E'#"#006BA6"'blue'

# linetype
STline<-"dash"
RHline<-"solid"
AirTline<-"soild"


kml <- readOGR(file, layer = ogrListLayers(file)[1])
#kml@coords[,'coords.x1']
c1 <- awesomeIcons(icon = "ios-close", iconColor = "black",
                   library = "ion", markerColor = "blue")

m <- leaflet()%>%#addSearchOSM()%>%
  htmlwidgets::onRender(".leaflet-control {
                        float: left;
                        clear: both;
                        }")
m <- m %>% addProviderTiles("OpenStreetMap.Mapnik", group = "OSM")
m <- m %>% addProviderTiles("Esri.WorldImagery", group = "SAT")
#m<-m %>%addCircles(data=kml,color = "darkblue",
#                   radius = 10,opacity = 1,popup= as.character(kml$Name))

m<-m %>%addAwesomeMarkers(data = kml,icon = c1,popup= as.character(kml$Name))

m <- m %>%addSearchOSM()%>%
  addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
                           onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
  addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
  addLayersControl(baseGroups = c("OSM","SAT"),
                   options = layersControlOptions(collapsed = FALSE),position = "topleft")


close(pb)
