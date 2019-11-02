#' Merge data of a location - using closest province of Bozen and SBR stations -
#'
#' @export
#' @importFrom glue glue_collapse
#' @importFrom dplyr full_join matches
#' @param long long input forgetClosestStations
#' @param lat lat input for getClosestStations
#' @param idSBR id of the SBR station input for getClosestStations
#' @param datestart starting date for download
#' @param dateend ending date for download
#' @param round rounding for resample. Default "day"
#' @param provSensor province sencors to dowload
#' @param sbrSensor if only data from the province of bozen is supplied se to NULL
#' @param mergeBoth if this is true all the measurements from SBR and province are downloaded and the mean is computed. Default FALSE. If true provSensor and sbrSensor are ingnored.

mergeData <- function(long=NULL,lat=NULL,idSBR=NULL,datestart=Sys.Date()-2,dateend=Sys.Date()+1,round="day",
                      provSensor=c("GS","N"),
                      sbrSensor=c("Temperatur 2m_min","Temperatur 2m_max",
                                  "Relative Luftfeuchtigkeit_min",
                                  "Relative Luftfeuchtigkeit_max",
                                  "Windgeschwindigkeit_avg"),
                      mergeBoth=FALSE,
                      password,user,host){

  if(mergeBoth){

    x<-getClosestStations(long = long,lat = lat,idSBR=idSBR,provSensor=provSensor)
    y<-get_provBz_data(station_sensor = x$prov,
                       datestart = datestart,
                       dateend = dateend,
                       spread = T,
                       round = round,notScode =T)
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
    colnames(z)[colnames(z)=="Relative Luftfeuchtigkeit_min"] <- "LF_min"
    colnames(z)[colnames(z)=="Relative Luftfeuchtigkeit_max"] <- "LF_max"
    colnames(z)[colnames(z)=="Temperatur 2m_max"] <- "LT_max"
    colnames(z)[colnames(z)=="Temperatur 2m_min"] <- "LT_min"
    colnames(z)[colnames(z)=="Windgeschwindigkeit_avg"] <- "WG_mean"
    colnames(z)[colnames(z)=="Niederschlag_sum"] <- "N_sum"

    a=bind_rows(y,z) %>% group_by(TimeStamp) %>%
      summarise_if(is.numeric,mean, na.rm = TRUE) %>%
      ungroup

  }else{

  x<-getClosestStations(long = long,lat = lat,idSBR=idSBR,provSensor = provSensor)

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
    colnames(z)[colnames(z)=="Niederschlag_sum"] <- "N_sum"
    #toTakeOut <- paste0("^",glue_collapse(paste0(provSensor,"_"),"|"))

    #z <- z %>% select(-matches(toTakeOut))

    a<-full_join(y,z,by="TimeStamp")
  }else{a=y}

  }
  return(a)

}
