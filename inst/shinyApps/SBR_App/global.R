has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

if(.Platform$OS.type=="windows"){

  pb = winProgressBar(
    title = sprintf('Starting %s ...', "dataBrowser"),
    label = 'Initializing ...'
  )
  setWinProgressBar(pb, 1.00, label = 'Starting application')

  if (!has_internet()) {

    msg=tcltk::tk_messageBox(
      type="ok",
      message="This app needs an active internet connection to work. Please check your internet connection",
      icon="error")

  }else{}

}

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
# irrigApple
library(ggplot2)
library(ggimage)
library(RMariaDB)
library(sp)
# irrigApple end
library(SBR)

Logged = FALSE

Host <- "95.171.35.104"

# irrigApple
round="hour"
provSensor=c("N","GS")#,"WG"
#long=11.457978
#lat=46.657158

today <- Sys.Date()
# irrigApple end


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


if(.Platform$OS.type=="windows"){
  close(pb)
}
