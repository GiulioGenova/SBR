if(.Platform$OS.type=="windows"){

  pb = winProgressBar(
    title = sprintf('Starting %s ...', "dataBrowser"),
    label = 'Initializing ...'
  )
  setWinProgressBar(pb, 1.00, label = 'Starting application')
}

# the server
library(shiny)
library(SBR)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggimage)
library(DBI)
library(RMariaDB)
library(shinycssloaders)
library(sp)
library(SBR)



round="hour"
provSensor=c("N","GS")#,"WG"
#long=11.457978
#lat=46.657158
Logged = FALSE
today <- Sys.Date()
Host <- "95.171.35.104"

if(.Platform$OS.type=="windows"){
  close(pb)
}
