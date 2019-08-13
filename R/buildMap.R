#' Create the leaflet Map
#'
#' @export
#' @import leaflet
#' @import leaflet.extras
#'

buildMap <- function(){
  html_legend <- "<img width=30 height=30 src='weather-station-icon-2.jpg'> <br/>Beratungsring<br/>Wetterstation"


  m <- leaflet() %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Street Map")%>%
    addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
    addSearchOSM()%>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Locate Me",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    addFullscreenControl()%>%
    addMarkers(lng = name_file$lon,lat = name_file$lat,
               popup = name_file$name,label = name_file$name,
               icon=makeIcon(iconUrl = "weather-station-icon-2.jpg",
                             iconWidth =  30,iconHeight =  30),
               clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = T)
    ) %>%
    # addCircleMarkers(lng = name_file$lon,lat = name_file$lat,
    #            popup = name_file$name,label = name_file$name#,
    #            #clusterOptions = markerClusterOptions(freezeAtZoom = 5)
    #            ) %>%
    addLayersControl(baseGroups = c("Satellite","Street Map"),#overlayGroups = c('draw'),
                     options = layersControlOptions(collapsed = FALSE),position = "topright")%>%
    addControl(html = html_legend, position = "topright")
  #m
  #levels=unique(orchards$Landuse)

  m <- m %>% # addPolygons(stroke= F,data = orchards,color = "blue" , fillOpacity = 0.35)%>%
    addPolygons(stroke= T,color = "black" , fillOpacity = 0,opacity = 1,weight = 2,
                data=mask) #%>%
  #addLegend(position = "topright",pal = "blue", values = levels, title = "Apple Orchards")
  m

}
