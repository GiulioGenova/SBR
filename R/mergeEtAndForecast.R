#' merge data from wheater forecast and et average data
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate yday
#' @importFrom dplyr mutate select left_join
#'

mergeEtAndForecast <- function(long,lat){

frcst<-forecast(long=long,lat = lat)

frcst<-frcst %>%  mutate(doy = yday(TimeStamp),
                         N_sum = (rainFrom+rainTo)/2) %>%
  select(doy,TimeStamp,N_sum)

df<-left_join(frcst,etAvg,by="doy") %>%
  select(- doy)

}
