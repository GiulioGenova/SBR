#' Create the leaflet Map
#'
#' @export
#' @import leaflet
#' @import leaflet.extras
#'

buildMap <- function(){

  m <- leaflet() %>%
    #setView( lng = 10.921895, lat = 46.458969,zoom = 10) %>%
    addSearchOSM()%>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Locate Me",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    addFullscreenControl()%>%

    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Street Map")%>%
    addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
    addLayersControl(baseGroups = c("Satellite","Street Map"),#overlayGroups = c('draw'),
                     options = layersControlOptions(collapsed = FALSE),position = "topright")
  #m
  #levels=unique(orchards$Landuse)

  m <- m %>%  addPolygons(stroke= F,data = orchards,color = "blue" , fillOpacity = 0.35) #%>%
  #addLegend(position = "topright",pal = "blue", values = levels, title = "Apple Orchards")
  m

}
