in progress

#' Get data from wheater forecas of province of Bozen sensors.
#'
#' @export
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#'
forecast <- function(){
  #http://daten.buergernetz.bz.it/it/dataset/southtyrolean-weatherservice-weatherdistricts/resource/aaf363af-7dad-423a-a137-662c57fd74b4
  url <- "http://daten.buergernetz.bz.it/services/weather/district/1/bulletin?format=json&lang=en"
  u <- GET(url) %>% content

  ui <- cbind(sapply(u, "[[", 1), sapply(u, "[[", 2)) %>%
    as.tibble

  #u$forecasts[[1]]$symbol <- NULL
  u[[4]][[1]][["symbol"]] <- NULL
  u[[4]][[1]][["symbol"]] <- NULL
  u[[4]][[2]][["symbol"]] <- NULL
  u[[4]][[3]][["symbol"]] <- NULL
  u[[4]][[4]][["symbol"]] <- NULL
  u[[4]][[5]][["symbol"]] <- NULL
  u[[4]][[6]][["symbol"]] <- NULL

  ui <- cbind(lapply(u[[4]], "[[", 1),
              lapply(u[[4]], "[[", 2),
              lapply(u[[4]], "[[", 3),
              lapply(u[[4]], "[[", 4),
              lapply(u[[4]], "[[", 5),
              lapply(u[[4]], "[[", 6),
              lapply(u[[4]], "[[", 7),
              lapply(u[[4]], "[[", 8),
              lapply(u[[4]], "[[", 9),
              lapply(u[[4]], "[[", 10),
              lapply(u[[4]], "[[", 11)) %>%
    as.tibble

  ui <- rbind(lapply(u[4], "[[", 1),
              lapply(u[4], "[[", 2),
            lapply(u[4], "[[", 3),
              lapply(u[4], "[[", 4),
              lapply(u[4], "[[", 5),
              lapply(u[4], "[[", 6),
              lapply(u[4], "[[", 7),
              lapply(u[4], "[[", 8),
              lapply(u[4], "[[", 9),
              lapply(u[4], "[[", 10),
              lapply(u[4], "[[", 11)) %>%
    as.tibble

  x<-u[4]

  y<-unlist(x,recursive = T,use.names = T)

  se<-bind_rows(y)


  se <-se[!duplicated(se[ , 1:2 ]), ]# %>% as.data.frame
  colnames(se)[colnames(se)=="TYPE"] <- "Sensor"
  return(se)

}
