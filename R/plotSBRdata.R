#' plot data form SBR stations with plotly
#'
#' @export
#' @import plotly
#' @import ggplot2
#'

plotSBRdata <- function(db,height=1500){

  # #db<-as_tibble(db)
  #
  # height =NULL# 850
  # #subplots<-function(station,db){
  # a<-plot_ly(db,x = ~TimeStamp, y = ~LF_mean,color = I(RHcolor),line = list(dash = RHline),height = height)%>%#
  #   add_lines(name=~"Air RH [%]")%>%
  #   layout(yaxis = RHy)
  # b<-plot_ly(db,x = ~TimeStamp, color=I(AirTmeancolor),line = list(dash = AirTline),height = height)%>%#
  #   add_lines(y = ~LT_mean,name=~"Air Tmean [°C]")%>%
  #   add_lines(y = ~LT_min,name=~"Air Tmin [°C]",color = I(AirTmincolor))%>%
  #   add_lines(y = ~LT_max,name=~"Air Tmax [°C]",color = I(AirTmaxncolor))%>%
  #   add_lines( y = ~BT20_mean,color=I(ST20color),line = list(dash = STline),name=~"Soil T 20cm [°C]")%>%
  #   add_lines(y = ~BT40_mean,color=I(ST40color),line = list(dash = STline),name=~"Soil T 40cm [°C]")%>%
  #   layout(yaxis = Airy)
  # c<-plot_ly(db,x = ~TimeStamp, y = ~WG_mean,color=I(Windcolor),height = height)%>%#
  #   add_lines(name=~"Wind speed [m/s]")%>%
  #   layout(yaxis = windy)
  # d<-plot_ly(db,x = ~TimeStamp, color = I(Pcolor),height = height)%>%#
  #   add_bars(y = ~N_sum,name=~"P [mm]")%>%
  #   add_bars(y = ~IR_sum,color = I( Irrigcolor),name=~"Irrig [mm]")%>%
  #   layout(yaxis = Py)
  # h<-db%>% mutate(BWP20_mean=replace(BWP20_mean,BWP20_mean>0,NA),BWP40_mean=replace(BWP40_mean,BWP40_mean>0,NA))%>%#filter(SWP_20cm<0&SWP_40cm<0)%>%
  #   plot_ly(x = ~TimeStamp, color=I(SWC20color),height = height)%>%#
  #   add_lines(y = ~BWP20_mean,line = list(dash = STline),name=~"SWP 20cm [kPa]")%>%
  #   add_lines(y = ~BWP40_mean,color=I(SWC40color),line = list(dash = STline),name=~"SWP 40cm [kPa]")%>%
  #   layout(yaxis = SWPy)
  # f<-plot_ly(db,x = ~TimeStamp, color=I(SWC20color),height = height)%>%#
  #   add_lines(y = ~BWC20_mean,name=~"SWC 20cm [%]")%>%
  #   add_lines(y = ~BWC40_mean,color=I(SWC40color),name=~"SWC 40cm [%]")%>%
  #   layout(yaxis = SWCy)
  #
  # #plot_list_a=list(a,b,c)
  # #plot_list_b=list(d,f,h)
  # #pa<-subplot(a,b,c,nrows = 3,shareX = T,titleX = F,titleY = TRUE)#%>%
  # # pb<-subplot(d,f,h,nrows = 3,shareX = T,titleX = F,titleY = TRUE)#%>%
  # #subplot(pa,pb,nrows = 2,shareX = T,titleX = F,titleY = TRUE)
  # #}
  #
  # #db<-lapply(station,subplots,db=db)
  #
  # #subplot(db,nrows =length(db))
  #
  # #subplot(a,d,b,f,c,h,nrows = 3,shareX = T,titleX = F,titleY = TRUE)
  # subplot(a,b,c,d,f,h,nrows = 6,shareX = T,titleX = F,titleY = TRUE)


  # p <- ggplot(db, aes(x = TimeStamp, y = Value,color=Sensor)) +
  #   geom_line(size = .75) +
  #   facet_rep_wrap(~ Sensor, ncol = 1,scales = "free_y", repeat.tick.labels = 'bottom')+
  #   theme_bw()
  # #hgt=paste0(as.character(length(unique(db$Sensor))*100),"px")
  #
  # p <- ggplotly(p,height = length(unique(db$Sensor))*250,dynamicTicks = T,)#
  #

  #p <-
  # db <- db %>% filter(!is.na(Value))
  #
  # p <- db %>%
  #   transform(id = as.integer(factor(Sensor))) %>%
  #   plot_ly(x = ~TimeStamp, y = ~Value, color = ~Sensor,
  #           height = height, colors = "Dark2",
  #           yaxis = ~paste0("y", id)) %>%
  #   add_lines() %>%
  #   #layout(autosize=TRUE) %>%#
  #   subplot(nrows = length(unique(db$Sensor)), shareX = TRUE,titleX = F,titleY = F)
  #
  # layout(p,autosize=TRUE)
  # #p
  db <- db %>% filter(!is.na(Value))

  plot_list <- function(db){#,height=800,nsensors
    #db=df
    sensors=unique(db$Sensor)
    #db <- db %>%  transform(ids = as.integer(factor(Sensor)))

    plots<- function(sensors,db){#,height=height,nsensors=nsensors

      db <- db %>%filter(Sensor==sensors)

      db %>% plot_ly(linetype = ~id,
        x = ~TimeStamp,
        y = ~Value#,
        #height = (height/nsensors)
      ) %>%
        add_lines(name =  unique(db$Sensor)) %>%#paste(unique(db$MesswertEh),)
        layout(yaxis = list(title = ~Sensor))#title= ~Sensor,
    }


    lapply(sensors,plots,db=db)#,height=height,nsensors=nsensors

  }

  sensors=unique(db$Sensor)
  subplot(plot_list(db=db),#,nsensors=nsensors
          nrows = length(sensors),shareX = TRUE,#,height = height
          titleX = F,titleY = TRUE)%>%
    layout(autosize = T, height = height)


}
