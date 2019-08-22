#' merge data from wheater forecast and et average data
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate yday
#' @importFrom dplyr mutate select left_join
#'

mergeEtAndForecast <- function(long,lat,slope=NULL){

frcst<-forecast(long=long,lat = lat)

frcst<-frcst %>%  mutate(doy = yday(TimeStamp),
                         N_sum = (rainFrom+rainTo)/2)# %>%
  #select(doy,TimeStamp,N_sum,img)

if(!is.null(slope)){
  corr=1/cos(slope*pi/180)
  etAvg$ET = etAvg$ET*corr
}

df<-left_join(frcst,etAvg,by="doy") %>%
  select(- doy)

}
