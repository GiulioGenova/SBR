
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "SBR App - Demo"),
  dashboardSidebar(
    sidebarMenu(

      menuItem("Data Browser", tabName = "Data", icon = icon("bar-chart-o")),
      menuItem("Station map", tabName = "map", icon = icon("info-circle")),
      menuItem("irrigAplant", tabName = "irrigApple", icon = icon("bar-chart-o")),
      menuItem("irrigApple Demo", tabName = "irrigAppleDm", icon = icon("bar-chart-o"))
    )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    # Boxes need to be put in a row (or column)
    tabItems(

      tabItem(tabName = "Data",

              fluidRow(
                box(width = 2,#,
                    selectInput("Station", label = h4(tags$b("Station")), #,multiple = T,
                                choices = sort(name_file$name),
                                selected = "Algund_2",multiple = T)),

                box(width = 3,#,
                    selectInput("selectedsensor", label = h4(tags$b("Parameter")),
                                choices = paste0(unique(sensor_file$MesswertBezDe),"_avg"),
                                selected = "Temperatur 2m_avg",multiple = T)#, as.list(unique(sensor_file$MesswertBezDe))
                    #verbatimTextOutput("selectSensorlist")
                ),

                box(width = 3,dateRangeInput(label = h4(tags$b("Datumsbereich auswählen")),
                                             inputId = "daterange",
                                             separator = " - ",
                                             min = "2013-01-01",
                                             start = Sys.Date()-3,
                                             end = Sys.Date()+1,
                                             language = "de"
                )),

                box(width = 2,selectInput("round",label = h4("Zeitaggregation"),
                                          choices = list(rohwerte="raw",
                                                         stündlich="hour",
                                                         täglich="day",
                                                         monatlich="month",
                                                         järlich="year"),
                                          selected = "hour")#,
                    # sliderInput('plotHeight', 'Höhe der Grafik (in Pixel)',
                    #             min = 150, max = 3500, value = 480)
                )
                ,

                box(width = 2,
                    sliderInput('plotHeight', 'Höhe der Grafik (in Pixel)',
                                min = 150, max = 3500, value = 480)
                ),

                box(width = 2,

                    conditionalPanel(condition = "output.rightstatsens==false",
                                     textOutput( "mssgstatsenserror")),

                    conditionalPanel(condition = "output.rightstatsens",#br(),
                                     actionButton(label= "Grafik/Auswahl aktualisieren","refresh"))
                )

                ,

                box(width = 2,
                    downloadLink('downloadData', h4('Download'))
                ),

                uiOutput("plotAll")
                # box(
                #   #id="box_plot",
                #   title = "Soil Air Precipitation Irrigation",solidHeader = TRUE,
                #   collapsible = FALSE,status = "primary",
                #   plotlyOutput("plotall")%>% withSpinner(), width = 12)#,height = "920px"
                #   #plotOutput("plotall",height = "auto")%>% withSpinner(), width = 12)#,height = "920px"

              )

      ),
      tabItem(tabName = "map",
              fluidRow(
                box(
                  id="box2",
                  width = 12,collapsible = T,#height="80vh",
                  htmlOutput("map2"))

              )
      ),
      tabItem(tabName = "irrigApple",
              fluidRow(
                column(
                  width = 5,
                  box(
                    width = 12,
                    p(h4("Geben Sie unten den letzten Tag an, den Sie bewässert haben.") ),
                    p(h4("Klicken Sie dann in der Karte auf Ihr Feld, um einen Bewässerungshinweis zu erhalten")),
                    dateInput("date",label = "Letzter Bewässerungszeitpunkt",
                              min = Sys.Date()-90,max = Sys.Date(),language = "de"),

                    # irrig used to be based on two categories ("normal" and "light")
                    # and computed based on the TAW. the client asked for a numerical
                    # input (in mm)

                    # radioButtons("irr", "Irrigation type:",
                    #              c("Normal" = "norm",
                    #                "Light" = "light"),
                    #              selected = "norm",
                    #              inline = T),

                    numericInput("irr", "Bewässerte Menge [mm]:",50),

                    numericInput("slope", "Steigung [%]:",
                                 0,min = 0,max = 35
                    ),

                    radioButtons("soil", "Bodenart:",
                                 c("Schwer" = "heavy",
                                   "Mittel"= "medium",
                                   "Licht" = "light"),
                                 selected = "medium",
                                 inline = T)

                  ),

                  box(
                    width=12,
                    plotOutput("irrigAdvise")%>% withSpinner()
                  )
                )
                ,
                column(
                  width = 7,
                  box(
                    id="box1",
                    width = 12,
                    leafletOutput("mapIrrig")#%>% withSpinner()
                  )
                )


              )
      ),
      tabItem(tabName = "irrigAppleDm",
              fluidRow(
                column(width = 5,
                       box(
                         width = 12,
                         p(h5("Please indicate the last day you irrigated below.") ),
                         p(h5("Then click in the map on your field to get irrigation advice")),
                         dateInput("dateDm",label = "last irrigation date",
                                   min = Sys.Date()-365,max = Sys.Date()),
                         dateInput("today",label = "what day is today",
                                   min = Sys.Date()-364,max = Sys.Date()),
                         radioButtons("irrDm", "Irrigation type:",
                                      c("Normal" = "norm",
                                        "Light" = "light"),
                                      selected = "norm",
                                      inline = T),
                         radioButtons("soilDm", "Soil type:",
                                      c("Heavy" = "heavy",
                                        "Medium"= "medium",
                                        "Light" = "light"),
                                      selected = "medium",
                                      inline = T)

                       ),

                       box(
                         width=12,
                         plotOutput("irrigAdviseDm")%>% withSpinner()

                       )
                )
                ,
                column(
                  width = 7,
                  box(
                    id="boxDm",
                    width = 12,
                    leafletOutput("mapIrrigDm")#%>% withSpinner()
                  )
                )


              )
      )

    )
  )
)
