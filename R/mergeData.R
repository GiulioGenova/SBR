#' Merge data of a location - using closest province of Bozen and SBR stations -
#'
#' @export
#' @importFrom glue glue_collapse
#' @importFrom dplyr full_join matches
#' @param sbrSensor if only data from the province of bozen is supplied se to NULL

mergeData <- function(long,lat,datestart=Sys.Date()-2,dateend=Sys.Date()+1,round="day",
                      provSensor=c("GS","N"),
                      sbrSensor=c("Temperatur 2m_min","Temperatur 2m_max",
                                  "Relative Luftfeuchtigkeit_min",
                                  "Relative Luftfeuchtigkeit_max",
                                  "Windgeschwindigkeit_avg"),
                      password,user,host){

  x<-getClosestStations(long = long,lat = lat,provSensor = provSensor)

  y<-get_provBz_data(station_sensor = x$prov,
                     datestart = datestart,
                     dateend = dateend,
                     spread = T,
                     round = round,notScode =T)
  if(!is.null(sbrSensor)){
    z<-get_BR_data(station = x$sbr,
                   sensor=sbrSensor,
                   datestart = datestart,
                   dateend = dateend,
                   user = user,
                   password = password,
                   host = host,
                   add_names = F,
                   spread = T,
                   round = round)

    #colnames(z)[colnames(z)==] <- "GS_mean"
    colnames(z)[colnames(z)=="Relative Luftfeuchtigkeit_min"] <- "LF_min"
    colnames(z)[colnames(z)=="Relative Luftfeuchtigkeit_max"] <- "LF_max"
    colnames(z)[colnames(z)=="Temperatur 2m_max"] <- "LT_max"
    colnames(z)[colnames(z)=="Temperatur 2m_min"] <- "LT_min"
    colnames(z)[colnames(z)=="Windgeschwindigkeit_avg"] <- "WG_mean"

    #toTakeOut <- paste0("^",glue_collapse(paste0(provSensor,"_"),"|"))

    #z <- z %>% select(-matches(toTakeOut))

    a<-full_join(y,z,by="TimeStamp")
  }else{a=y}
  return(a)

}
