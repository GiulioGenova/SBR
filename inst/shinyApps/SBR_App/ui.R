
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "SBR App - Demo"),
  dashboardSidebar(
    sidebarMenu(

      menuItem("Data Browser", tabName = "Data", icon = icon("bar-chart-o")),
      menuItem("Station map", tabName = "map", icon = icon("info-circle")),
      menuItem("irrigAplant", tabName = "irrigApple", icon = icon("bar-chart-o")),
      menuItem("irrigAplant Demo", tabName = "irrigAppleDm", icon = icon("bar-chart-o"))
    )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    # Boxes need to be put in a row (or column)
    tabItems(

      tabItem(tabName = "Data",

              fluidRow(
                box(width = 3,

                    selectInput("Station", label = h4(tags$b("Station")), #,multiple = T,
                                choices = sort(name_file$name),
                                selected = "Algund_2",multiple = T),

                    conditionalPanel(condition = "output.rightstatsens==false",
                                     textOutput( "mssgstatsenserror")),


                    conditionalPanel(condition = "output.rightstatsens",#br(),
                                     actionButton(label= "Grafik/Auswahl aktualisieren","refresh"))

                ),

                box(width = 3,

                    selectInput("selectedsensor", label = h4(tags$b("Parameter")),
                                choices = paste0(unique(sensor_file$MesswertBezDe),"_avg"),
                                selected = "Temperatur 2m_avg",multiple = T),# as.list(unique(sensor_file$MesswertBezDe))
                    #verbatimTextOutput("selectSensorlist")
                    conditionalPanel(
                      condition = "input.round != 'raw'",
                      checkboxGroupInput(inputId="doResamp",label = "Zeitaggregation",
                                         choices = list(Avg="_avg",
                                                        Min="_min",
                                                        Max="_max",
                                                        Sum="_sum"),
                                         selected = "_avg",
                                         inline=TRUE)
                    )
                    # checkboxInput("doAvg","Avg",TRUE),
                    # checkboxInput("doSum","Sum",TRUE),
                    # checkboxInput("doMin","Min",TRUE),
                    # checkboxInput("doMax","Max",TRUE)
                ),

                box(width = 3,
                    dateRangeInput(label = h4(tags$b("Datumsbereich auswählen")),
                                   inputId = "daterange",
                                   separator = " - ",
                                   min = "2013-01-01",
                                   start = Sys.Date()-3,
                                   end = Sys.Date()+1,
                                   language = "de"
                    ),

                    sliderInput('plotHeight', 'Höhe der Grafik (in Pixel)',
                                min = 150, max = 3500, value = 480)

                ),

                box(width = 3,

                    selectInput("round",label = h4("Zeitaggregation"),
                                choices = list(Rohwerte="raw",
                                               stündlich="hour",
                                               täglich="day",
                                               monatlich="month",
                                               jährlich="year"),
                                selected = "hour"),

                    downloadLink('downloadData', h4('Download'))

                )
                ,

                fluidRow(
                  uiOutput("plotAll")
                  # box(
                  #   #id="box_plot",
                  #   title = "Soil Air Precipitation Irrigation",solidHeader = TRUE,
                  #   collapsible = FALSE,status = "primary",
                  #   plotlyOutput("plotall")%>% withSpinner(), width = 12)#,height = "920px"
                  #   #plotOutput("plotall",height = "auto")%>% withSpinner(), width = 12)#,height = "920px"

                )
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
                conditionalPanel(id = "controlsParent",
                                 condition="($('html').hasClass('shiny-busy'))",
                                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                               draggable = TRUE,top = 15, left = "auto", right = 0, bottom = "auto",
                                               img(src="spinner3.gif",
                                                   width = "150px", height = "220px")
                                 )
                ),
                # column(
                #   width = 3,
                #   box(
                #     width = 12,
                #     p(h4("Geben Sie unten den letzten Tag an, den Sie bewässert haben.") ),
                #     p(h4("Klicken Sie dann in der Karte auf Ihr Feld, um einen Bewässerungshinweis zu erhalten")),
                #     dateInput("date",label = "Letzter Bewässerungszeitpunkt",
                #               min = Sys.Date()-90,max = Sys.Date(),language = "de"),
                #
                #     # irrig used to be based on two categories ("normal" and "light")
                #     # and computed based on the TAW. the client asked for a numerical
                #     # input (in mm)
                #
                #     # radioButtons("irr", "Irrigation type:",
                #     #              c("Normal" = "norm",
                #     #                "Light" = "light"),
                #     #              selected = "norm",
                #     #              inline = T),
                #
                #     numericInput("irr", "Bewässerte Menge [mm]:",value = 50,min = 0,max=300),
                #
                #     # numericInput("slope", "Steigung [%]:",
                #     #              0,min = 0,max = 35
                #     # ),
                #
                #     radioButtons("soil", "Bodenart:",
                #                  c("Schwer" = "heavy",
                #                    "Mittel"= "medium",
                #                    "Leicht" = "light"),
                #                  selected = "medium",
                #                  inline = T)
                #
                #   )
                # )
                # ,
                column(
                  width = 12,
                  # box(
                  #   width=12,
                  #   #plotOutput("irrigAdvise")%>% withSpinner()
                  #   timevisOutput("irrigAdvise")
                  # )
                  box(
                    width = 6,
                    p(h4("Geben Sie unten den letzten Tag an, den Sie bewässert haben.") ),
                    p(h4("Klicken Sie dann in der Karte auf Ihr Feld, um einen Bewässerungshinweis zu erhalten")),
                    dateInput("date",label = "Letzter Bewässerungszeitpunkt",
                              min = Sys.Date()-90,max = Sys.Date(),language = "de")#,

                    # irrig used to be based on two categories ("normal" and "light")
                    # and computed based on the TAW. the client asked for a numerical
                    # input (in mm)

                    # radioButtons("irr", "Irrigation type:",
                    #              c("Normal" = "norm",
                    #                "Light" = "light"),
                    #              selected = "norm",
                    #              inline = T),
                  ),
                  box(
                    width = 6,

                    numericInput("irr", "Bewässerte Menge [mm]:",value = 50,min = 0,max=300),

                    # numericInput("slope", "Steigung [%]:",
                    #              0,min = 0,max = 35
                    # ),

                    radioButtons("soil", "Bodenart:",
                                 c("Schwer" = "heavy",
                                   "Mittel"= "medium",
                                   "Leicht" = "light"),
                                 selected = "medium",
                                 inline = T)

                  ),
                  box(
                    width = 12,
                    conditionalPanel(condition="output.nodata",
                                     p(h3("no data") )
                    ),
                    conditionalPanel(condition="output.nodata==false",
                                     timevisOutput("irrigAdvise")
                    )
                  )
                  ,
                  box(
                    id="box1",
                    width = 12,
                    leafletOutput("mapIrrig")#,
                    # absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                    #               draggable = TRUE, top = 5, left = "auto", right = 80, bottom = "auto",
                    #               width = "80%", height = "auto",
                    #
                    #               timevisOutput("irrigAdvise")
                    #
                    # )
                  )
                )


              )
      ),
      tabItem(tabName = "irrigAppleDm",
              fluidRow(
                column(width = 5,
                       box(
                         width = 12,
                         p(h4("Geben Sie unten den letzten Tag an, den Sie bewässert haben.") ),
                         p(h4("Klicken Sie dann in der Karte auf Ihr Feld, um einen Bewässerungshinweis zu erhalten")),

                         # dateInput("dateDm",label = "Letzter Bewässerungszeitpunkt",
                         #           min = Sys.Date()-365,max = Sys.Date(),language = "de"),
                         # dateInput("today",label = "Welcher Tag ist heute?",
                         #           min = Sys.Date()-364,max = Sys.Date(),language = "de"),

                         # radioButtons("irrDm", "Irrigation type:",
                         #              c("Normal" = "norm",
                         #                "Light" = "light"),
                         #              selected = "norm",
                         #              inline = T),


                         div(style=" width: 100%;",
                             div(style="display: inline-block;vertical-align:top; width: 40%;",
                                 dateInput("dateDm",label = "Letzter Bewässerungszeitpunkt",
                                           min = Sys.Date()-365,max = Sys.Date(),language = "de")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 40%;",
                                 dateInput("today",label = "Welcher Tag ist heute?",
                                           min = Sys.Date()-364,max = Sys.Date(),language = "de")
                             )
                         ),


                         numericInput("irrDm", "Bewässerte Menge [mm]:",value = 50,min = 0,max=300),

                         radioButtons("soilDm", "Bodenart:",
                                      c("Schwer" = "heavy",
                                        "Mittel"= "medium",
                                        "Leicht" = "light"),
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
