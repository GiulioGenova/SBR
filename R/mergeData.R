#' Merge data of a location - using closest province of Bozen and SBR stations -
#'
#' @export
#' @importFrom glue collapse
#' @importFrom dplyr full_join matches
#'

mergeData <- function(long,lat,datestart=Sys.Date()-2,dateend=Sys.Date()+1,round="day",
                      provSensor=c("GS","N"),password,user,host){

  x<-getClosestStations(long = long,lat = lat,provSensor = provSensor)

  y<-get_provBz_data(station_sensor = x$prov,
                     datestart = datestart,
                     dateend = dateend,
                     spread = T,
                     round = round,notScode =T)

  z<-get_BR_data(station = x$sbr,
                 datestart = datestart,
                 dateend = dateend,
                 user = user,
                 password = password,
                 host = host,
                 add_names = F,
                 spread = T,
                 round = round)

  toTakeOut <- collapse(paste0(provSensor,"_"),"|")

  z <- z %>% select(-matches(toTakeOut))

  a<-full_join(y,z,by="TimeStamp")

  return(a)

}
