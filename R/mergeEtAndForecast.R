#' merge data from wheater forecast and et average data
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate yday
#' @importFrom dplyr mutate select left_join
#'

mergeEtAndForecast <- function(long,lat,slope=NULL,
                               Kc.ini.A=0.1,
                               Kc.ini.B=0.4,
                               Kc.dev=0.7,
                               Kc.mid=1.2,
                               Kc.late=0.8,
                               DOY.ini.A=69,
                               DOY.ini.B=71,
                               DOY.dev=91,
                               DOY.mid=141,
                               DOY.late=271,
                               DOY.harv=312){

frcst<-forecast(long=long,lat = lat)

frcst<-frcst %>%  mutate(doy = yday(TimeStamp),
                         N_sum = (rainFrom+rainTo)/2)# %>%
  #select(doy,TimeStamp,N_sum,img)

if(!is.null(slope)){
  corr=1/cos(slope*pi/180)
  etAvg$ET = etAvg$ET*corr
}

df<-left_join(frcst,etAvg,by="doy") %>%
  mutate(DOY=doy,
         Kc = ifelse(DOY == DOY.ini.A, Kc.ini.A,
                     ifelse(DOY %in% DOY.ini.A:DOY.ini.B,(Kc.ini.B-Kc.ini.A)/(DOY.ini.B-DOY.ini.A)*(DOY-DOY.ini.A)+Kc.ini.A,
                            ifelse(DOY %in% DOY.ini.B:DOY.dev,(Kc.dev-Kc.ini.B)/(DOY.dev-DOY.ini.B)*(DOY-DOY.ini.B)+Kc.ini.B,
                                   ifelse(DOY %in% DOY.dev:DOY.mid, (Kc.mid-Kc.dev)/(DOY.mid-DOY.dev)*(DOY-DOY.dev)+Kc.dev,
                                          ifelse(DOY %in% DOY.mid:DOY.late, Kc.mid,
                                                 ifelse(DOY %in% DOY.late:DOY.harv,(Kc.late-Kc.mid)/(DOY.harv-DOY.late)*(DOY-DOY.late)+Kc.mid, 0)))))),
         ET0 = ifelse(Kc>0.001,ET/Kc,0)
         ) %>%
  select(- doy,-DOY)

return(df)

}
