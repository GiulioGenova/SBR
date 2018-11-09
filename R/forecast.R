#' Get data from wheater forecas of province of Bozen sensors.
#'
#' @export
#' @importFrom httr GET content
#' @importFrom magrittr %>%
#' @importFrom lubridate as_date
#' @importFrom tibble as.tibble
#'
forecast <- function(){
  #http://daten.buergernetz.bz.it/it/dataset/southtyrolean-weatherservice-weatherdistricts/resource/aaf363af-7dad-423a-a137-662c57fd74b4
  url <- "http://daten.buergernetz.bz.it/services/weather/district/1/bulletin?format=json&lang=en"
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

  return(ui)

}
