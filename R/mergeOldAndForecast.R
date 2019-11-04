#' merge et and precipitation data from wheater forecast and dayly measured et data
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate yday
#' @importFrom dplyr mutate select left_join bind_rows
#'


mergeOldAndForecast <- function(data,long,lat,slope=NULL,...){

  dff<-mergeEtAndForecast(long = long,lat = lat,slope=slope,...)

  colnames(dff)[colnames(dff)=="ET"] <- "ETc"

  dfo<- data %>%
    filter(TimeStamp<Sys.Date())

  df<-bind_rows(dfo,dff)

  return(df)
}
