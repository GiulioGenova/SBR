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
library(sp)
library(shinycssloaders)
library(SBR)



round="hour"
provSensor=c("N","GS")#,"WG"

Logged = FALSE
#today <- Sys.Date()
Host <- "95.171.35.104"

if(.Platform$OS.type=="windows"){
  close(pb)
}
