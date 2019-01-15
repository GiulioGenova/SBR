
server <- function(input, output, session) {

  values <- reactiveValues(authenticated = FALSE)

  # Return the UI for a modal dialog with data selection input. If 'failed'
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      #textInput("host", "Host:"),
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
      #Host <- input$host
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

    staticMap

  })

  #leafletOutput('map', height=600)
  observe({
    #station=sub("\\_.*", "", input$Station)
    station=names_file[names_file$name==input$Station,"id"]
    query <- as.character(paste(station,collapse="+"))
    #test <<- sprintf(fmt = "http://www.beratungsring.org/wetterdaten/map_popup.php?ST=%s&date=%s",query,as.character(Sys.Date(),format="%d.%m.%Y"))
    test <- sprintf(fmt = "http://www.beratungsring.org/wetterdaten/map_popup.php?ST=%s&date=%s",query,as.character(Sys.Date(),format="%d.%m.%Y"))
    assign("test", test, envir=globalenv())

  })

  output$map2 <- renderUI({

    #station=sub("\\_.*", "", input$Station)
    station=names_file[names_file$name==input$Station,"id"]
    map_test <- tags$iframe(src=test, height="100%", width="100%")
    print(map_test)
    map_test
  })


  ## connenction and query on sbr mysql database
  inputdb=reactive({
    #station=sub("\\_.*", "", input$Station)
    station=names_file[names_file$name==input$Station,"id"]
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
    #station=sub("\\_.*", "", input$Station)

    plotSBRdata(db)

  })


  output$plotall <- renderPlotly({
    pp()
  })

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
  # irrigApple

  latLong<-reactiveValues(lat=NULL,long=NULL)

  #values <- reactiveValues(authenticated = FALSE)


  observe({

    click <- input$mapIrrig_click
    latLong$lat <- click$lat
    latLong$long <- click$lng

  })

  observe({

    lat <- as.numeric(latLong$lat)
    long <- as.numeric(latLong$long)

    proxy <- leafletProxy("mapIrrig")%>% clearMarkers() #%>% removeDrawToolbar(clearFeatures = TRUE)

    proxy<- proxy %>%

      addAwesomeMarkers(lng = long, lat = lat)


  })


  db <- reactive({
    req(input$date)
    req(input$mapIrrig_click)
    datestart <- input$date
    #datestart <- "2018-11-01"
    #long=11.857978
    #lat=46.657158

    irr<- input$irr
    soil<- input$soil

    if(soil=="heavy"){
      TAW=85
    }else if(soil=="medium"){
      TAW=75
    }else if(soil=="light"){
      TAW=65
    }

    if(irr=="norm"){
      startwb=TAW
    }else if(irr=="light"){
      startwb=TAW*0.35
    }

    lat <- latLong$lat
    long <- latLong$long

    point <- cbind(LONG=long,LAT=lat)
    point <- SpatialPoints(point,proj4string = CRS("+init=epsg:4326"))

    fallsin<- !is.na(point %over% orchards [,"Landuse"])[1]
    if(fallsin){
      db <- mergeData(long = long,lat = lat,
                      datestart = datestart,
                      #dateend = Sys.Date()+1,
                      provSensor = provSensor,
                      password = password,user = user,host = host)

      et <- ET(data = db,crop = "tall")

      wb <- WB(et,taw = TAW,startwb = startwb)

      wb <- mergeOldAndForecast(data = wb,long = long,lat = lat)

      wb <- wb %>% filter(TimeStamp > today)
    } else {

      wb<-NULL
    }
  })




  output$irrigAdvise <- renderPlot({
    req(db())

    db <- db()

    plotIrrigAdvice(db,T)

  })



  output$mapIrrig <- renderLeaflet({
    buildMap()
  })
  # irrigApple end

  # irrigAppleDemo
  latLongDm<-reactiveValues(lat=NULL,long=NULL)

  observe({

    clickDm <- input$mapIrrigDm_click
    latLongDm$lat <- clickDm$lat
    latLongDm$long <- clickDm$lng

  })

  observe({

    lat <- as.numeric(latLongDm$lat)
    long <- as.numeric(latLongDm$long)

    proxy <- leafletProxy("mapIrrigDm")%>% clearMarkers() #%>% removeDrawToolbar(clearFeatures = TRUE)

    proxy<- proxy %>%

      addAwesomeMarkers(lng = long, lat = lat)


  })

  dbDm <- reactive({
    req(input$dateDm)
    req(input$mapIrrigDm_click)
    datestart <- input$dateDm
    #datestart <- "2018-11-01"



    irrDm<- input$irrDm
    soilDm<- input$soilDm

    lat <- latLongDm$lat
    long <- latLongDm$long
    today <- input$today

    if(soilDm=="heavy"){
      TAW=85
    }else if(soilDm=="medium"){
      TAW=75
    }else if(soilDm=="light"){
      TAW=65
    }

    if(irrDm=="norm"){
      startwb=TAW
    }else if(irrDm=="light"){
      startwb=TAW*0.35
    }



    point <- cbind(LONG=long,LAT=lat)
    point <- SpatialPoints(point,proj4string = CRS("+init=epsg:4326"))

    fallsin<- !is.na(point %over% orchards [,"Landuse"])[1]
    if(fallsin){


      db <- mergeData(long = long,lat = lat,
                      datestart = datestart,
                      dateend = today+5,
                      provSensor = provSensor,
                      password = password,user = user,host = host)

      et <- ET(data = db,crop = "tall")

      #df <- mergeOldAndForecast(data = et,long = long,lat = lat)

      wb <- WB(et,taw = TAW,startwb = startwb)

      wb <- wb %>% filter(TimeStamp > today)
    } else {

      wb<-NULL
    }
  })




  output$irrigAdviseDm <- renderPlot({
    req(dbDm())

    db <- dbDm()

    plotIrrigAdvice(db,F)

  })

  output$mapIrrigDm <- renderLeaflet({
    buildMap()
  })
  # irrigAppleDemo end

  #outputOptions(output, "irrigAdvise", suspendWhenHidden = FALSE)

  session$onSessionEnded(function() {
    stopApp()
  })

}