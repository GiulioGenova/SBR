
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "irrigAplant"),
  dashboardSidebar(collapsed = T,
    sidebarMenu(
      menuItem("irrigAplant", tabName = "irrigApple", icon = icon("bar-chart-o"))

    )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    # Boxes need to be put in a row (or column)
    tabItems(

      tabItem(tabName = "irrigApple",
              fluidRow(
                conditionalPanel(id = "controlsParent",
                                 condition="($('html').hasClass('shiny-busy'))",
                                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                               draggable = TRUE,top = 25, left = "auto", right = 0, bottom = "auto",
                                               img(src="annaffiatoio.gif",#spinner3.gif
                                                   width = "200px", height = "200px")
                                 )
                ),

                column(
                  width = 12,

                  box(
                    width = 6,
                    p(h4("Geben Sie unten den letzten Tag an, den Sie bewässert haben.") ),
                    p(h4("Klicken Sie dann in der Karte auf Ihr Feld, um einen Bewässerungshinweis zu erhalten")),
                    dateInput("date",label = "Letzter Bewässerungszeitpunkt",
                              min = Sys.Date()-90,max = Sys.Date(),language = "de")
                  ),

                  box(

                    fluidRow(
                      column(
                        width = 4,
                        numericInput("irr", "Bewässerte Menge [mm]:",
                                     value = 50,min = 0,max=300)

                        # numericInput("slope", "Steigung [%]:",
                        #              0,min = 0,max = 35
                        # ),
                      ),
                      column(width = 4,
                             radioButtons("soil", "Bodenart:",
                                          c("Schwer" = "heavy",
                                            "Mittel"= "medium",
                                            "Leicht" = "light"),
                                          selected = "medium",
                                          inline = F)
                      )
                      ,
                      column(width = 4,
                             radioButtons("dataSource", "Datenquelle:",
                                          c(
                                            "Beratungsring"= "sbr",
                                            "Provinz" = "prov",
                                            "Beide" = "beide"),
                                          selected = "sbr",
                                          inline = F)
                      )
                    )
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
                    leafletOutput("mapIrrig")
                  )
                )
              )
      )
    )
  )
)
