# the ui


#tags$head(tags$link(rel="shortcut icon", href="/www/favicon.png"))

navbarPage(

  title = "IrrigApple",

  #theme = "bootstrap.css",

  tabPanel("Map",
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
           ),
           div(class="outer",style="position: fixed;top: 41px;
                        left: 0;right: 0;bottom: 0;
                        overflow: hidden;padding: 0;",
               leafletOutput("map", width = "100%", height = "100%"), #
               absolutePanel(id = "controls", class = "panel panel-default",
                             fixed = TRUE,
                             draggable = TRUE, top = "20%", left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto", cursor = "move",
                             br(),
                             p("Please indicate the last day you irrigated below."),
                             p("Then click in the map on your field to get irrigation advice"),
                             dateInput("date",label = "last irrigation date",
                                       min = Sys.Date()-180,max = Sys.Date()),
                             dateInput("today",label = "what day is today",
                                       min = Sys.Date()-180,max = Sys.Date()),
                             plotOutput("irrigAdvise", height = "250px")%>% withSpinner()

               )
           )
  )#,

  # tabPanel("About",
  #          fluidRow(
  #            column(12,
  #                   wellPanel(
  #                     includeMarkdown("about.md"))
  #            )
  #          )
  # )

)

