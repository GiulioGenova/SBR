#' Get data from wheater forecas of province of Bozen sensors based on lat long locations.
#'
#' @export
#' @importFrom httr GET content
#' @importFrom magrittr %>%
#' @importFrom lubridate as_date
#' @importFrom tibble as.tibble
#' @importFrom sp  %over% SpatialPoints spTransform CRS
#'
forecast <- function(long,lat){
  #http://daten.buergernetz.bz.it/it/dataset/southtyrolean-weatherservice-weatherdistricts/resource/aaf363af-7dad-423a-a137-662c57fd74b4


  crs <- districts@proj4string

  point <- cbind(LONG=long,LAT=lat)
  point <- SpatialPoints(point,proj4string = CRS("+init=epsg:4326"))
  point <- spTransform(point, CRS = crs)


  over <- point%over%districts

  if (over$BEZ==1) {

    dstr <- 3 # "Val Venosta"

  }else if (over$BEZ==2){

    dstr <- 2 # "Burgraviato - Merano e dintorni"

  }else if (over$BEZ==7){

    dstr <- 5 # "Val d´Isarco e Val Sarentino"

  }else if (over$BEZ%in%c(3,4,5)){

    dstr <- 1 # "Bolzano, Oltradige e Bassa Atesina"

  }else if (over$BEZ==6){

    dstr <- 4 # "Val d´Isarco e Val Sarentino"

  }else if(over$BEZ==8){

    dstr <- 6 # "Val Pusteria"

  }

  url <- sprintf("http://daten.buergernetz.bz.it/services/weather/district/%s/bulletin?format=json&lang=en",
          dstr)

  #url <- "http://daten.buergernetz.bz.it/services/weather/district/1/bulletin?format=json&lang=en"
  u <- GET(url) %>% content

  ui <- cbind(sapply(u[[4]], "[[", "date"),
              sapply(u[[4]], "[[", "rainFrom"),
              sapply(u[[4]], "[[", "rainTo"),
              sapply(u[[4]], "[[", "temperatureMax"),
              sapply(u[[4]], "[[", "temperatureMin")
              ) %>%
    as.tibble
  names(ui) <- c("date","rainFrom","rainTo","temperatureMax","temperatureMin")
  ui$date <- as_date(ui$date,tz="Europe/Berlin")
  ui$rainFrom <- as.numeric(ui$rainFrom)
  ui$rainTo <- as.numeric(ui$rainTo)
  colnames(ui)[colnames(ui)=="date"] <- "TimeStamp"
  return(ui)

}
