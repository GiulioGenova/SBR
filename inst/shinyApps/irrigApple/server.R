

server <- function(input, output,session) {

  latLong<-reactiveValues(lat=NULL,long=NULL)

  values <- reactiveValues(authenticated = FALSE)

  # Return the UI for a modal dialog with data selection input. If 'failed'
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      #textInput("host", "Host:"),

      if (failed)
        div(tags$b("Invalid username or password", style = "color: red;")),

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
                host = Host,
                user =  Username,
                password = Password)

    }, error = function(e){NULL})


    if (!is.null(Logged)) {

      Logged <<- TRUE
      #values$authenticated <- TRUE
      obs1$suspend()
      removeModal()
      user <<- Username
      password <<- Password
      host <<- Host
      dbDisconnect(Logged)

    } else {

      #values$authenticated <- FALSE
      showModal(dataModal(failed = TRUE))
    }

  })


  # output$dataInfo <- renderPrint({
  #   if (values$authenticated) "OK!!!!!"
  #   else "You are NOT authenticated"
  # })



  observe({

    click <- input$map_click
    latLong$lat <- click$lat
    latLong$long <- click$lng

  })

  observe({

    lat <- as.numeric(latLong$lat)
    long <- as.numeric(latLong$long)

    proxy <- leafletProxy("map")%>% clearMarkers() #%>% removeDrawToolbar(clearFeatures = TRUE)

    proxy<- proxy %>%

      addAwesomeMarkers(lng = long, lat = lat)


  })


  db <- reactive({
    req(input$date)
    req(input$map_click)
    datestart <- input$date
    #datestart <- "2018-11-01"
    #long=11.857978
    #lat=46.657158
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

      et <- ET(data = db)

      df <- mergeOldAndForecast(data = et,long = long,lat = lat)

      wb <- WB(df)

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



  output$map <- renderLeaflet({
    buildMap()
  })

  session$onSessionEnded(function() {
    stopApp()
  })

}
