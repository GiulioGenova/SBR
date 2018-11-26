
library(ggplot2)
library(DBI)
library(RMariaDB)
library(dplyr)
#library(dbplyr)
library(lubridate)
library(plotly)
library(shinydashboard)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr)
library(rgdal)
library(shinycssloaders)
library(tidyr)

Logged = FALSE

wd<-getwd()
file<-file.path(wd,"Orchards_monitoring_station_MONALISA-SBR_Project_21032018.kml")

# y axis title
RHy <- list(title = "Air RH [%]")
Airy <- list(title = "Air & Soil T [°C]")
Py <- list(title = "P & Irrig [mm]")
windy <- list(title = "Wind speed [m/s]")
SWCy <- list(title = "SWC [%]")
SWPy<- list(title = "SWP [kPa]")
STy <- list(title = "Soil T [°C]")

# colors
RHcolor<-"#0188AE"
AirTmeancolor<-"#000A10"
AirTmaxncolor<-"#D81159"
AirTmincolor<-"#0496FF"
Pcolor<-"#1778AE"
Irrigcolor<-"#00273D"
Windcolor<-"#0D0508"
SWC20color<-"#8F2D56"##same for SWP'red'
SWC40color<-"#006BA6"#'blue'#same for SWP
ST20color<-'#FFBC42'#"#8F2D56"'red'
ST40color<-'#74561E'#"#006BA6"'blue'

# linetype
STline<-"dash"
RHline<-"solid"
AirTline<-"soild"


kml <- readOGR(file, layer = ogrListLayers(file)[1])
#kml@coords[,'coords.x1']
c1 <- awesomeIcons(icon = "ios-close", iconColor = "black",
                   library = "ion", markerColor = "blue")

m <- leaflet()%>%#addSearchOSM()%>%
  htmlwidgets::onRender(".leaflet-control {
                        float: left;
                        clear: both;
                        }")
m <- m %>% addProviderTiles("OpenStreetMap.Mapnik", group = "OSM")
m <- m %>% addProviderTiles("Esri.WorldImagery", group = "SAT")
#m<-m %>%addCircles(data=kml,color = "darkblue",
#                   radius = 10,opacity = 1,popup= as.character(kml$Name))

m<-m %>%addAwesomeMarkers(data = kml,icon = c1,popup= as.character(kml$Name))

m <- m %>%addSearchOSM()%>%
  addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
                           onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
  addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
  addLayersControl(baseGroups = c("OSM","SAT"),
                   options = layersControlOptions(collapsed = FALSE),position = "topleft")



server <- function(input, output) {

  values <- reactiveValues(authenticated = FALSE)

  # Return the UI for a modal dialog with data selection input. If 'failed'
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      textInput("host", "Host:"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }

  # Show modal when button is clicked.
  # This `observe` is suspended only whith right user credential

  obs1 <- observe({
    showModal(dataModal())
  })

  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal.

  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
      Host <- input$host
    })


    Logged = tryCatch({
      #user<-"wrong"
      dbConnect(MariaDB(),#RMariaDB::
                dbname = sprintf('sbr_wetter_%s',substr(Sys.Date(),1,4)),
                host = Host,user =  Username,
                password = Password)

    }, error = function(e){NULL})


    if (!is.null(Logged)) {

      Logged <<- TRUE
      values$authenticated <- TRUE
      obs1$suspend()
      removeModal()
      user <<- Username
      password <<- Password
      host <<- Host
      dbDisconnect(Logged)

    } else {

      values$authenticated <- FALSE

    }

  })


  output$dataInfo <- renderPrint({
    if (values$authenticated) "OK!!!!!"
    else "You are NOT authenticated"
  })

  output$map<-renderLeaflet({

    m

  })

  #leafletOutput('map', height=600)
  observe({
    station=sub("\\_.*", "", input$Station)

    query <- as.character(paste(station,collapse="+"))
    test <- sprintf(fmt = "http://www.beratungsring.org/wetterdaten/map_popup.php?ST=%s&date=%s",query,as.character(Sys.Date(),format="%d.%m.%Y"))
  })

  output$map2 <- renderUI({

    station=sub("\\_.*", "", input$Station)
    map_test <- tags$iframe(src=test, height="100%", width="100%")
    print(map_test)
    map_test
  })


  ## connenction and query on sbr mysql database
  inputdb=reactive({
    station=sub("\\_.*", "", input$Station)
    start_date<-as.character(input$daterange[1])
    end_date<-as.character(input$daterange[2])
    out_dir="H:/Projekte/SBR/04_Data/06_daily_resample"
    round=input$round
    #db<-resample_SBR(station = station,start_date = start_date,end_date = end_date,round=round)
    db_SBR<-get_BR_data(station=station,datestart=start_date,dateend=end_date,
                        user = user,password = password,
                        host=host,
                        spread = T,
                        round=round)

    #db<-resample_BR_data(db_SBR,round = round,spread=T)

  })



  # pp <- eventReactive(c(input$refreshColours,input$slider2,input$slider1),{
  #  ggplot(dt, aes(x,y)) +
  #   geom_point(size=input$slider1, alpha=input$slider2, colour=isolate(input$check1))
  #})



  ## Plot output
  pp <- eventReactive(input$refresh,{#,input$daterange,input$round,input$Station
    db=inputdb()
    station=sub("\\_.*", "", input$Station)
    #db<-as_tibble(db)


    #subplots<-function(station,db){
    a<-plot_ly(db,x = ~TimeStamp, y = ~LF_mean,color = I(RHcolor),line = list(dash = RHline))%>%#
      add_lines(name=~"Air RH [%]")%>%
      layout(yaxis = RHy)
    b<-plot_ly(db,x = ~TimeStamp, color=I(AirTmeancolor),line = list(dash = AirTline))%>%#
      add_lines(y = ~LT_mean,name=~"Air Tmean [°C]")%>%
      add_lines(y = ~LT_min,name=~"Air Tmin [°C]",color = I(AirTmincolor))%>%
      add_lines(y = ~LT_max,name=~"Air Tmax [°C]",color = I(AirTmaxncolor))%>%
      add_lines( y = ~BT20_mean,color=I(ST20color),line = list(dash = STline),name=~"Soil T 20cm [°C]")%>%
      add_lines(y = ~BT40_mean,color=I(ST40color),line = list(dash = STline),name=~"Soil T 40cm [°C]")%>%
      layout(yaxis = Airy)
    c<-plot_ly(db,x = ~TimeStamp, y = ~WG_mean,color=I(Windcolor))%>%#
      add_lines(name=~"Wind speed [m/s]")%>%
      layout(yaxis = windy)
    d<-plot_ly(db,x = ~TimeStamp, color = I(Pcolor))%>%#
      add_bars(y = ~N_sum,name=~"P [mm]")%>%
      add_bars(y = ~IR_sum,color = I( Irrigcolor),name=~"Irrig [mm]")%>%
      layout(yaxis = Py)
    h<-db%>% mutate(BWP20_mean=replace(BWP20_mean,BWP20_mean>0,NA),BWP40_mean=replace(BWP40_mean,BWP40_mean>0,NA))%>%#filter(SWP_20cm<0&SWP_40cm<0)%>%
      plot_ly(x = ~TimeStamp, color=I(SWC20color))%>%#
      add_lines(y = ~BWP20_mean,line = list(dash = STline),name=~"SWP 20cm [kPa]")%>%
      add_lines(y = ~BWP40_mean,color=I(SWC40color),line = list(dash = STline),name=~"SWP 40cm [kPa]")%>%
      layout(yaxis = SWPy)
    f<-plot_ly(db,x = ~TimeStamp, color=I(SWC20color))%>%#
      add_lines(y = ~BWC20_mean,name=~"SWC 20cm [%]")%>%
      add_lines(y = ~BWC40_mean,color=I(SWC40color),name=~"SWC 40cm [%]")%>%
      layout(yaxis = SWCy)

    #plot_list_a=list(a,b,c)
    #plot_list_b=list(d,f,h)
    #'%ni%' <- Negate('%in%')
    #pa<-subplot(a,b,c,nrows = 3,shareX = T,titleX = F,titleY = TRUE)#%>%
   # pb<-subplot(d,f,h,nrows = 3,shareX = T,titleX = F,titleY = TRUE)#%>%
    #subplot(pa,pb,nrows = 2,shareX = T,titleX = F,titleY = TRUE)
    #}

    #db<-lapply(station,subplots,db=db)

    #subplot(db,nrows =length(db))

    subplot(a,d,b,f,c,h,nrows = 3,shareX = T,titleX = F,titleY = TRUE)
  })


  output$plotall <- renderPlotly({
    pp()
  })

  #output$plotair <- renderPlotly({
  #  db=inputdb()
  # station=sub("\\_.*", "", input$Station)
  #db<-as_tibble(db)


  # a<-plot_ly(db,x = ~date, y = ~RHmean)%>%#
  #add_lines(name=~"RHmean")

  #ay <- list(
  # tickfont = list(color = "red"),
  # overlaying = "y",
  # side = "right",
  #title = "second y axis"
  # )
  # p <- plot_ly() %>%
  #   add_lines(db,x = ~date, y = ~RHmean, name = "RH") %>%
  #   add_lines(db,x = ~date, y = ~Tmean, name = "T", yaxis = "y2") %>%
  #  layout(
  #    title = "Double Y Axis", yaxis2 = ay,
  #    xaxis = list(title="x")
  #  )
  #   a

  #})

  output$downloadData <- downloadHandler(

    filename = function() {
      db=inputdb()
      station=as.character(input$Station)
      startdate<-as.character(input$daterange[1])
      enddate<-as.character(input$daterange[2])
      round=input$round
      paste0(station,'_',round,'_',startdate,'_',enddate,'.csv')
    },
    content = function(con) {
      db=inputdb()
      write.csv(db, con,quote = F,row.names = F,na = "NA",sep = ",",dec = ".")
    }
  )




}
