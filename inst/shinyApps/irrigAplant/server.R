
server <- function(input, output, session) {

  values <- reactiveValues(authenticated = FALSE)

  # Return the UI for a modal dialog with data selection input. If 'failed'
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      p(h4(tags$b("Welcome to the SBR App"))),
      p(h5("Please insert username and password for the Beratungsring database")),
      p(h5("You need to be inside Beratungsring or eurac network to use this app")),
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


  # irrigApple

  latLong<-reactiveValues(lat=NULL,long=NULL)

  #values <- reactiveValues(authenticated = FALSE)


  observe({

    click <- input$mapIrrig_click
    latLong$lat <- click$lat
    latLong$long <- click$lng

  })

  datSource<-reactiveValues(provSensor=NULL,sbrSensor=NULL,mergeBoth=NULL)

  observe({
    if(input$dataSource=="prov"){
      datSource$provSensor = c("GS","N","WG","LT","LF")
      datSource$sbrSensor = NULL
      datSource$mergeBoth = FALSE
    }else if(input$dataSource=="sbr"){
      datSource$provSensor = c("GS")
      datSource$sbrSensor = c("Temperatur 2m_min", "Temperatur 2m_max",
                              "Relative Luftfeuchtigkeit_min", "Relative Luftfeuchtigkeit_max",
                              "Windgeschwindigkeit_avg","Niederschlag_sum")
      datSource$mergeBoth = FALSE
    }else {
      datSource$provSensor = c("GS","N","WG","LT","LF")
      datSource$sbrSensor = c("Temperatur 2m_min", "Temperatur 2m_max",
                              "Relative Luftfeuchtigkeit_min", "Relative Luftfeuchtigkeit_max",
                              "Windgeschwindigkeit_avg","Niederschlag_sum")
      datSource$mergeBoth = TRUE
    }
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
    lat <- latLong$lat
    long <- latLong$long
    #datestart <- "2018-11-01"
    #long=11.857978
    #lat=46.657158

    point <- cbind(LONG=long,LAT=lat)
    #point <- SpatialPoints(point,proj4string = CRS("+init=epsg:4326"))
    point <- st_sfc(st_point(point),crs = 4326)
    #fallsin<- !is.na(point %over% mask )[1]#[,"Landuse"]
    fallsin<-length(st_intersects(point,st_as_sf(mask))[[1]])!=0
    if(fallsin){
      db <- mergeData(long = long,lat = lat,
                      datestart = datestart,
                      #dateend = Sys.Date()+1,
                      provSensor = datSource$provSensor,
                      sbrSensor = datSource$sbrSensor,
                      mergeBoth = datSource$mergeBoth,
                      password = password,user = user,host = host)

      slope=raster::extract(slopeFilt, as(point,"Spatial"))

      et <- ET(data = db,crop = "tall",slope=slope,latitude=lat)

      wb <- mergeOldAndForecast(data = et,long = long,lat = lat,slope=slope)

    } else {

      wb<-NULL
    }
  })

  dbWb <- reactive({

    req(db())

    irr<- input$irr
    soil<- input$soil

    # irrig used to be based on two categories ("normal" and "light")
    # and computed based on the TAW. the client asked for a numerical
    # input (in mm)

    irrig=as.numeric(irr)

    if(soil=="heavy"){
      TAW=85
    }else if(soil=="medium"){
      TAW=75
    }else if(soil=="light"){
      TAW=65
    }

    wb <- WB(db(),taw = TAW,irrig = irrig)

  })


  output$nodata <-reactive({
    return(is.null(db()))
  })


  output$irrigAdvise <- renderTimevis({
    req(dbWb())

    db <- dbWb()

    plotIrrigAdvice2(db,T)

  })

  output$mapIrrig <- renderLeaflet({
    buildMap()
  })

  session$onSessionEnded(function() {
    stopApp()
  })
  outputOptions(output, 'nodata', suspendWhenHidden=FALSE)
}
