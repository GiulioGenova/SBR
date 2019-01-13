library(SBR)
library(ggplot2)
library(dplyr)
library(lubridate)
password='roMonaLisa$14pr'
user='ROeuracMonalisa'
host='95.171.35.104'

round="day"
#datestart=Sys.Date()-4
datestart="2018-03-10"
dateend="2018-10-15"

stat="Salurn_2"
long=names_file[names_file$name==stat,]$lon
lat=names_file[names_file$name==stat,]$lat
taw = 50;lmitWarning = 0.8;p = 0.5
provSensor=c("GS")#,"WG","LF","LT""N",




sbr_model <- read.csv("wb_2018_Salurn_2.csv",stringsAsFactors = F)
sbr_model$TimeStamp <- lubridate::as_datetime(sbr_model$TimeStamp,tz="Europe/Berlin",
                                              format="%d/%m/%Y")
sbr_model$TimeStamp <- as.POSIXct(sbr_model$TimeStamp)



db <- mergeData(long = long,lat = lat,
                datestart = datestart,
                dateend = dateend,
                provSensor = provSensor,
                password = password,user = user,host = host)

et <- ET(db)


#df <- mergeOldAndForecast(data = et,long = long,lat = lat)
#wb <- WB(et,irrig=et$IRYN_h*4,startwb = 50,taw = 50)
et_irr<-left_join(et,sbr_model)
et_irr$irrig <- ifelse(is.na(et_irr$irrig),0,et_irr$irrig)
et_irr$N_sum <- ifelse(is.na(et_irr$N_sum),0,et_irr$N_sum)
wb <- WB(et_irr,startwb = 0,taw = 50,lmitWarning = lmitWarning,p = p)

wb$irrigAdvise_sbr=ifelse(wb$wb_sbr<=taw*p*lmitWarning,"NoIrrig",
                   ifelse(wb$wb_sbr>= taw*p*lmitWarning & wb$wb_sbr< taw*p,
                          "SugIrrig","MustIrrig"))

wb <- wb %>% mutate_if(is.numeric, funs(round), 2)

# to see if the advise is coherent with swp

wb %>% #filter(TimeStamp> as_datetime("2018-06-20") & TimeStamp< as_datetime("2018-09-01"))%>%
ggplot2::ggplot(aes(irrigAdvise,BWP20_mean))+
  geom_boxplot()+
  theme_classic()

wb %>% filter(TimeStamp> as_datetime("2018-06-20") & TimeStamp< as_datetime("2018-09-01"))%>%

  ggplot2::ggplot(aes(irrigAdvise_sbr,BWP20_mean))+
  geom_boxplot()+
  theme_classic()

wb %>% #filter(TimeStamp> as_datetime("2018-07-01") & TimeStamp< as_datetime("2018-09-01"))%>%
  ggplot2::ggplot(aes(wb,BWP20_mean,color=as.factor(month(TimeStamp))))+
  geom_point()+
  theme_classic()

wb %>% filter(TimeStamp> as_datetime("2018-06-20") & TimeStamp< as_datetime("2018-09-01"))%>%
ggplot2::ggplot(aes(wb,BWP20_mean))+#,color=as.factor(month(TimeStamp))
  geom_point()+
  geom_smooth()+
  theme_classic()

wb %>% filter(TimeStamp< as_datetime("2018-07-01") | TimeStamp> as_datetime("2018-09-01"))%>%
  ggplot2::ggplot(aes(wb,BWP20_mean))+
  geom_point()+
  theme_classic()

wb %>% filter(TimeStamp> as_datetime("2018-07-01") & TimeStamp< as_datetime("2018-09-01"))%>%
ggplot2::ggplot(aes(wb_sbr,BWP20_mean))+
  geom_point()+
  theme_classic()

ggplot2::ggplot(wb,aes(wb,BWP40_mean))+
  geom_point()+
  theme_classic()

ggplot2::ggplot(wb,aes(wb_sbr,BWP40_mean))+
  geom_point()+
  theme_classic()

# the water balances
wb %>% filter(N_sum<50) %>%
  ggplot2::ggplot(aes(TimeStamp,wb))+
  geom_line()+
  geom_line(aes(TimeStamp,wb_sbr),color="red")+
  geom_col(aes(TimeStamp,N_sum),fill="blue")+
  geom_col(aes(TimeStamp,irrig),fill="black")+
  theme_classic()

# the et balances
ggplot2::ggplot(wb,aes(TimeStamp,etcadj))+
  geom_line()+
  geom_line(aes(TimeStamp,et_sbr),color="blue")+
  geom_line(aes(TimeStamp,ETc),color="orange")+
  theme_classic()
