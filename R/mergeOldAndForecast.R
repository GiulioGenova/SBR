#' merge et and precipitation data from wheater forecast and dayly measured et data
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate yday
#' @importFrom dplyr mutate select left_join bind_rows
#'


mergeOldAndForecast <- function(data,long,lat){

  dff<-mergeEtAndForecast(long = long,lat = lat)

  colnames(dff)[colnames(dff)=="ET"] <- "ETc"

  dfo<- data %>% select(TimeStamp,N_sum,ETc) %>%
    filter(TimeStamp<Sys.Date())


  df<-bind_rows(dfo,dff)

}
