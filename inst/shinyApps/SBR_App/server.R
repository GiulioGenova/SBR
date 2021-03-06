
sensorPresel<-c("Trockentemperatur 60cm","Feuchttemperatur 60cm",
                "Relative Luftfeuchtigkeit","Temperatur 2m",
                "Windgeschwindigkeit","Windrichtung",
                "Blattnaesse","Niederschlag",
                "Beregnung",
                "Bodenfeuchtigkeit 20cm",
                "Bodenfeuchtigkeit 40cm","Bodenwasserpotenzial 20cm",
                "Bodenwasserpotenzial 40cm","Bodentemperatur -10cm",
                "Bodentemperatur -30cm","Luftdruck"
)#"Bodentemperatur -25cm","Verdunstung",

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


  ##############################################################################
  # data browser

  output$map<-renderLeaflet({

    staticMap

  })

  #leafletOutput('map', height=600)
  observe({
    #station=sub("\\_.*", "", input$Station)
    station=name_file[name_file$name%in%input$Station,"id"]
    query <- as.character(paste(station,collapse="+"))
    #test <<- sprintf(fmt = "http://www.beratungsring.org/wetterdaten/map_popup.php?ST=%s&date=%s",query,as.character(Sys.Date(),format="%d.%m.%Y"))
    test <- sprintf(fmt = "http://www.beratungsring.org/wetterdaten/map_popup.php?ST=%s&date=%s",query,as.character(Sys.Date(),format="%d.%m.%Y"))
    assign("test", test, envir=globalenv())

  })

  output$map2 <- renderUI({

    #station=sub("\\_.*", "", input$Station)
    station=names_file[names_file$name%in%input$Station,"id"]
    map_test <- tags$iframe(src=test, height="100%", width="100%")
    print(map_test)
    map_test
  })


  ## connenction and query on sbr mysql database
  inputdb=eventReactive(input$refresh,{
    #station=sub("\\_.*", "", input$Station)
    station=names_file[names_file$name%in%input$Station,"id"]
    start_date<-as.character(input$daterange[1])
    end_date<-as.character(input$daterange[2])
    #out_dir="H:/Projekte/SBR/04_Data/06_daily_resample"
    round=input$round
    #db<-resample_SBR(station = station,start_date = start_date,end_date = end_date,round=round)
    #db_SBR<-

    sensor=input$selectedsensor

    if(length(sensor)==0 | length(station)==0){

      NULL

    }else{

      get_BR_data(station=station,sensor=sensor,
                  datestart=start_date,dateend=end_date,
                  user = user,password = password,
                  host=host,
                  spread = F,
                  add_names = T,
                  round=round)

    }
    #db<-resample_BR_data(db_SBR,round = round,spread=T)

  })

  output$plotAll <- renderUI({

    #req(db())
    #db=inputdb()
    # if(is.character(pp())){
    #
    #   output$plotall <- renderText({
    #
    #     pp()
    #     #plotSBRdata(db)
    #   })
    #
    #
    #   box(width = 12,height = input$plotHeight+90,
    #       title = "Grafik",solidHeader = TRUE,
    #       #actionButton(label= "Grafik/Auswahl aktualisieren","refresh"),
    #       #sliderInput('plotHeight', 'Höhe der Grafik (in Pixel)',
    #       #           min = 150, max = 3500, value = 480),
    #       verbatimTextOutput("plotall")%>% withSpinner()#,
    #       #downloadLink('downloadData', h4('Download'))
    #   )
    #
    # }else{

    output$plotall <- renderPlotly({

      pp()
      #plotSBRdata(db)
    })

    #req(input$refresh)

    box(width = 12,height = input$plotHeight+90,
        title = "Grafik",solidHeader = TRUE,
        #actionButton(label= "Grafik/Auswahl aktualisieren","refresh"),
        #sliderInput('plotHeight', 'Höhe der Grafik (in Pixel)',
        #           min = 150, max = 3500, value = 480),
        plotlyOutput("plotall")%>% withSpinner()#,
        #downloadLink('downloadData', h4('Download'))
    )
    #}
  })


  # output$selectSensorlist <- renderText({
  #
  #   paramChoices()
  #
  # })


  output$rightstatsens <- reactive({

    station=names_file[names_file$name%in%input$Station,"id"]
    sensor=input$selectedsensor

    if(length(sensor)==0 | length(station)==0){

      FALSE

    }else{

      TRUE
    }

  })


  observe({

    #stat="Latsch_1"
    #stat=input$Station
    station=SBR::name_file[SBR::name_file$name%in%input$Station,"id"]

    tab <- SBR::sensor_file %>%
      filter(StationsNr%in%(station),
             MesswertBezDe%in%(sensorPresel))

    if(input$round=="raw"){

      x<-as.list(unique(tab$MesswertBezDe))

    }else{

      #if (input$doAvg){

      aggrAvg<-c("Trockentemperatur 60cm","Feuchttemperatur 60cm",
                 "Relative Luftfeuchtigkeit","Temperatur 2m",
                 "Windgeschwindigkeit","Windrichtung",
                 "Blattnaesse",
                 "Bodenfeuchtigkeit 20cm",
                 "Bodenfeuchtigkeit 40cm","Bodenwasserpotenzial 20cm",
                 "Bodenwasserpotenzial 40cm","Bodentemperatur -10cm",
                 "Bodentemperatur -30cm","Luftdruck")

      #}else {

      #  aggrAvg<-c()

      #}

      #if (input$doSum){

      aggrSum<-c("Niederschlag",
                 "Beregnung")
      #}else{

      #  aggrSum<-c()

      #}

      #if (input$doMin){

      aggrMin<-c("Trockentemperatur 60cm","Feuchttemperatur 60cm","Temperatur 2m",
                 "Bodentemperatur -10cm",
                 "Bodentemperatur -30cm")
      #}else{

      #  aggrMin<-c()

      #}

      #if (input$doMax){

      aggrMax<-c("Trockentemperatur 60cm","Feuchttemperatur 60cm","Temperatur 2m",
                 "Bodentemperatur -10cm",
                 "Bodentemperatur -30cm")

      # }else{

      #  aggrMax<-c()

      # }

      # x<-as.list(c(
      #
      #
      #   paste0(unique(tab$MesswertBezDe[tab$MesswertBezDe %in% aggrAvg]),"_avg"),
      #   paste0(unique(tab$MesswertBezDe[tab$MesswertBezDe %in% aggrSum]),"_sum"),
      #   paste0(unique(tab$MesswertBezDe[tab$MesswertBezDe %in% aggrMin]),"_min"),
      #   paste0(unique(tab$MesswertBezDe[tab$MesswertBezDe %in% aggrMax]),"_max"))
      # )

      x<-as.list(

        sort(as.vector(outer(tab$MesswertBezDe, input$doResamp, paste, sep="")),decreasing = F,na.last = T)
        #paste0(unique(tab$MesswertBezDe),input$doResamp)
      )

    }

    #return(choices)

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)

    # Can also set the label and select items
    updateSelectInput(session, "selectedsensor",
                      #label = paste("Select input label", length(x)),
                      choices = x,
                      selected = input$selectedsensor#"Temperatur 2m_avg"
    )
  })



  # pp <- eventReactive(c(input$refreshColours,input$slider2,input$slider1),{
  #  ggplot(dt, aes(x,y)) +
  #   geom_point(size=input$slider1, alpha=input$slider2, colour=isolate(input$check1))
  #})



  ## Plot output
  pp <- reactive({#,input$daterange,input$round,input$Station

    db=inputdb()
    #station=sub("\\_.*", "", input$Station)
    if(!is.null(db)){

      height=input$plotHeight
      plotSBRdata(db=db,height=height)#

    }else{

      "Select at least one station and one parameter"

    }

  })

  output$mssgstatsenserror <- renderText({
    "Wählen Sie mindestens eine Station und einen Parameter aus"
  })
  # output$plotall <- renderPlotly({
  #   pp()
  # })

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

  #############################################################################
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
  # irrigApple end

  #############################################################################
  # irrigAppleDemo
  latLongDm<-reactiveValues(lat=NULL,long=NULL)

  observe({

    clickDm <- input$mapIrrigDm_click
    latLongDm$lat <- clickDm$lat
    latLongDm$long <- clickDm$lng

  })

  datSourceDm<-reactiveValues(provSensor=NULL,sbrSensor=NULL,mergeBoth=NULL)

  observe({
    if(input$dataSourceDm=="prov"){
      datSourceDm$provSensor = c("GS","N","WG","LT","LF")
      datSourceDm$sbrSensor = NULL
      datSourceDm$mergeBoth = FALSE
    }else if(input$dataSourceDm=="sbr"){
      datSourceDm$provSensor = c("GS")
      datSourceDm$sbrSensor = c("Temperatur 2m_min", "Temperatur 2m_max",
                              "Relative Luftfeuchtigkeit_min", "Relative Luftfeuchtigkeit_max",
                              "Windgeschwindigkeit_avg","Niederschlag_sum")
      datSourceDm$mergeBoth = FALSE
    }else {
      datSourceDm$provSensor = c("GS","N","WG","LT","LF")
      datSourceDm$sbrSensor = c("Temperatur 2m_min", "Temperatur 2m_max",
                              "Relative Luftfeuchtigkeit_min", "Relative Luftfeuchtigkeit_max",
                              "Windgeschwindigkeit_avg","Niederschlag_sum")
      datSourceDm$mergeBoth = TRUE
    }
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

    lat <- latLongDm$lat
    long <- latLongDm$long
    today <- input$today

    point <- cbind(LONG=long,LAT=lat)
    #point <- SpatialPoints(point,proj4string = CRS("+init=epsg:4326"))
    point <- st_sfc(st_point(point),crs = 4326)
    #fallsin<- !is.na(point %over% mask )[1]#[,"Landuse"]
    fallsin<-length(st_intersects(point,st_as_sf(mask))[[1]])!=0
    if(fallsin){


      db <- mergeData(long = long,lat = lat,
                      datestart = datestart,
                      dateend = today+5,
                      provSensor = datSourceDm$provSensor,
                      sbrSensor = datSourceDm$sbrSensor,
                      mergeBoth = datSourceDm$mergeBoth,
                      password = password,user = user,host = host)

      slope=raster::extract(slopeFilt, as(point,"Spatial"))

      et <- ET(data = db,crop = "tall",slope=slope,latitude=lat)

      #df <- mergeOldAndForecast(data = et,long = long,lat = lat)

      #wb <- WB(et,taw = TAW,irrig = irrig)

      #wb <- wb %>% filter(TimeStamp > today)
    } else {

      wb<-NULL
    }
  })

  dbDmWb <- reactive({

    req(dbDm())

    irrDm<- input$irrDm
    soilDm<- input$soilDm
    irrig=as.numeric(irrDm)

    if(soilDm=="heavy"){
      TAW=85
    }else if(soilDm=="medium"){
      TAW=75
    }else if(soilDm=="light"){
      TAW=65
    }

    wb <- WB(dbDm(),taw = TAW,irrig = irrig)

  })

  output$nodataDm <-reactive({
    return(is.null(dbDm()))
  })


  output$irrigAdviseDm <- renderTimevis({
    req(dbDmWb())

    db <- dbDmWb()

    plotIrrigAdvice2(db,T)

  })


  output$mapIrrigDm <- renderLeaflet({
    buildMap()
  })
  # irrigAppleDemo end

  #outputOptions(output, "irrigAdvise", suspendWhenHidden = FALSE)
  outputOptions(output, "plotAll", suspendWhenHidden = FALSE)
  outputOptions(output, 'rightstatsens', suspendWhenHidden=FALSE)

  session$onSessionEnded(function() {
    stopApp()
  })
  outputOptions(output, 'nodata', suspendWhenHidden=FALSE)
  outputOptions(output, 'nodataDm', suspendWhenHidden=FALSE)
}
