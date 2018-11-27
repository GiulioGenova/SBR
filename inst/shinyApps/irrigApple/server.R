# the server
library(SBR)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggimage)
library(DBI)
library(RMariaDB)
library(SBR)

round="hour"
provSensor=c("N","GS")#,"WG"
#long=11.457978
#lat=46.657158
Logged = FALSE

server <- function(input, output,session) {

  latLong<-reactiveValues(lat=NULL,long=NULL)

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
    lat <- latLong$lat
    long <- latLong$long

    db <- mergeData(long = long,lat = lat,
                    datestart = datestart,
                    #dateend = Sys.Date()+1,
                    provSensor = provSensor,
                    password = password,user = user,host = host)

    et <- ET(data = db)

    df <- mergeOldAndForecast(data = et,long = long,lat = lat)

    df <- df %>% filter(TimeStamp > Sys.Date())

    wb <- WB(df)

  })




  output$irrigAdvise <- renderPlot({
    req(db())

    db <- db()

    # ggplot2::ggplot(db(),aes(x=TimeStamp,y=wb,color=irrigAdvise))+
    #   geom_line()+
    #   #geom_rect(aes(xmin = min(TimeStamp), xmax = max(TimeStamp),
    #   #              ymin = -Inf, ymax = Inf, fill = irrigAdvise), alpha = 0.4)+
    #   geom_hline(yintercept=0,size=50,color=irrigAdvise)


    #db$img<-"http://daten.buergernetz.bz.it/services/weather/graphics/icons/imgsource/wetter/icon_18.png"
    db <- db %>% mutate(TimeStamp = factor(TimeStamp),
                        TimeStamp = factor(TimeStamp,
                                          levels = rev(levels(TimeStamp))))

    p <- ggplot(db, aes(y=TimeStamp)) +#
      labs(x=NULL,y=NULL,fill=NULL)+
      geom_tile(aes(x="1",fill=irrigAdvise,width=1),#hjust = 0.1,, height=1
                color="white",show.legend = T,size=1.5)#
    #coord_fixed(ratio = 1)

    p + geom_image(x = 1.8, aes(image = img),size=.15) +
      #coord_flip() +
      #expand_limits(x = c(1,1.1))  +
      coord_cartesian(xlim = c(0.98, 2),expand = F)+
      #scale_y_reverse()+
      scale_fill_manual(values=c("Irrig"="red", "NoIrrig"="green"))+
      #coord_flip()+
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_blank(),#element_line(linetype = "dashed",colour = "grey"),
            axis.text.x=element_blank(),
            axis.text.y=element_text(face = "plain",size = 20,colour = "black"),
            axis.title.y = element_blank(),
            axis.title.x =  element_blank(),
            #axis.title.x = element_text(size = axis_title_size),#,margin = margin(r=20,l=20)
            legend.position = "top",#,

            # legend.title = element_text(size=legend_text_size),
            legend.text = element_text(size = 20)
            # legend.key = element_rect(colour = "#f1fafc"),#size = 2
            # legend.key.size = unit(3, 'lines'),
            # axis.line.x = element_line(colour = "black",size=1.2)
      )



  })



  output$map <- renderLeaflet({
    m <- leaflet() %>% setView(lng = 10.921895, lat = 46.458969, zoom = 10) %>%
      addSearchOSM()%>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Locate Me",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addFullscreenControl()%>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Street Map")%>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
      addLayersControl(baseGroups = c("Street Map","Satellite"),#overlayGroups = c('draw'),
                       options = layersControlOptions(collapsed = FALSE),position = "topright")
    m

  })

  session$onSessionEnded(function() {
    stopApp()
  })

}
