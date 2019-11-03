#' Computes the water balance for a certain station (by id or lat long) and saves it in a data.frame.
#' both data form the beratungsring or the province of Bozen can be used. With mergeBoth = TRUE both are used and averaged.
#'
#' @export
#' @importFrom dplyr filter
#' @param long long input forgetClosestStations
#' @param lat lat input for getClosestStations
#' @param idSBR id of the SBR station input for getClosestStations
#' @param datestart starting date for download
#' @param dateend ending date for download
#' @param round rounding for resample. Default "day"
#' @param provSensor province sencors to dowload
#' @param sbrSensor if only data from the province of bozen is supplied se to NULL
#' @param mergeBoth if this is true all the measurements from SBR and province are downloaded and the mean is computed. Default FALSE. If true provSensor and sbrSensor are ingnored.
#' @param password pasword for beratungsring database
#' @param user user for beratungsring database
#' @param host host for beratungsring database
#' @param crop to compute ET. "short" or "tall". Default "tall"
#' @param taw the total available water. Default 50
#' @param lmitWarning fraction of the taw below which the app suggests to irrigate. Default 0.8
#' @param p fraction of the taw defining the raw (readly available water)
#' @param startwb is the initial water balance. Default value is NULL which yields taw*p
#' @param irrig vector with irrigation. Same lenght as rows of data or one number for the starting irrigation (first day of the time series)
#' @param trgtM a date. if not NULL (default) retains data later than that date
#'
#'

water_balance_by_station=function(long = NULL,lat = NULL,idSBR = NULL,
                    datestart,dateend,round="day",
                    provSensor,sbrSensor,mergeBoth=FALSE,
                    password,user,host,
                    crop = "tall",
                    taw = 50,lmitWarning= 0.8,p = 0.5,
                    startwb = NULL,irrig = 50,trgtM=NULL){

  db <- mergeData(long = long,lat = lat,idSBR=idSBR,
                  datestart = datestart,
                  dateend = dateend,
                  round = round,
                  provSensor = provSensor,
                  sbrSensor = sbrSensor,
                  mergeBoth = mergeBoth,
                  password = password,user = user,host = host)

  et <- ET(data = db,crop = crop)

  wb <- WB(et,taw = taw,lmitWarning = lmitWarning,p = p,
           startwb = startwb,irrig = irrig)
  wb$name=SBR::name_file[which(name_file$id==idSBR),"name"]

  if(!is.null(trgtM)){ wb = wb %>% filter(TimeStamp >= trgtM) }

  return(wb)
}
