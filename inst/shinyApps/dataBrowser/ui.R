
library(ggplot2)
library(DBI)
library(dplyr)
library(lubridate)
library(plotly)
library(shinydashboard)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr)
library(rgdal)
library(shinycssloaders)
#library(shinyjs)
#install.packages("shinyjs")
#jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
#var element = document.documentElement,
#enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
#exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
#if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
#enterFS.call(element);
#} else {
#exitFS.call(document);
#}
#}'
ui <- dashboardPage(#useShinyjs(),
                    #extendShinyjs(text = jsToggleFS),
                    skin = "blue",
                    dashboardHeader(title = "SBR stations Data"),
                    dashboardSidebar(
                      sidebarMenu(

                        menuItem("Data overwiev", tabName = "Data", icon = icon("bar-chart-o")),
                        menuItem("map", tabName = "map", icon = icon("info-circle"))#,
                        #menuItem("Data detail", tabName = "detail", icon = icon("bar-chart-o"))
                      )),
                    dashboardBody(tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                  tags$style(type = "text/css", "#map2 {height: calc(100vh - 350px) !important;}"),
                                  tags$style(type = "text/css", "#plotall {height: calc(100vh - 200px) !important;}"),
                                  #tags$style(type = "text/css", "#plotair {height: calc(100vh - 200px) !important;}"),
                                  # Boxes need to be put in a row (or column)
                                  tabItems(

                                    tabItem(tabName = "Data",

                                            fluidRow(
                                              box(
                                                width = 12,height = "100%",collapsible = T,
                                                htmlOutput("map2")),
                                            # box(width = 2,selectInput("Station", label = h4("Station"),multiple = T,
                                             #                          choices = list('Nalls_2','Girlan_1','Gries_2','Unterrain_2','Terlan_3',
                                              #                                        'Kaltern_3','Terlan_3a','Algund_2','Plars','Lana_6_NEU',
                                              #                                       'Eppan','StPauls','Latsch_1','Salurn_1','Salurn_2','Latsch_3','Latsch_4',
                                               #                                       'Toell','Neumarkt_Stiermoos','Tramin_13er','Vill') )),

                                             box(width = 2,selectInput("Station", label = h4("Station"), #,multiple = T,
                                                                       choices = list("37_Algund_2",
                                                                                      "97_Eyrs",
                                                                                      "7_Girlan_1",
                                                                                      "179_Glurns",
                                                                                      "9_Gries_2",
                                                                                      "52_Lana_6_NEU",
                                                                                      "103_Latsch_1",
                                                                                      "105_Latsch_3",
                                                                                      "106_Latsch_4",
                                                                                      "3_Nals_2",
                                                                                      "169_Neumarkt_Stiermoos",
                                                                                      "135_Raas",
                                                                                      "171_Salurn_1",
                                                                                      "172_Salurn_2",
                                                                                      "14_Terlan_3",
                                                                                      "174_Tramin_13er",
                                                                                      "12_Unterrain_2",
                                                                                      "176_Vill"),selected = '3_Nalls_2' )),



                                             #
                                              #box(width = 2,selectInput("Date", label = h4("Year"),
                                               #                         choices = list("2012","2013","2014","2015","2016","2017","2018"),selected = year(Sys.Date()) )),
                                              #dateInput("date_start","Start date",format = "dd/mm"),#,max = as.Date(paste0(input$Date,"-12-31"))max = "2018-12-31",min = "2014-01-01"
                                              #dateInput("date_end","End date",format = "dd/mm"),#max = "2018-12-31",min = "2014-01-01"
                                              #box(width = 2,selectInput("date_start",label = h5("Start day"),choices =list("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"),selected = substr(Sys.Date()-1,9,10))),
                                              #box(width = 2,selectInput("month_start",label = h5("Start month"),choices = list("01","02","03","04","05","06","07","08","09","10","11","12"),selected = substr(Sys.Date()-1,6,7))),
                                              #box(width = 2,selectInput("date_end",label = h5("End day"),choices = list("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"),selected = substr(Sys.Date(),9,10))),
                                              #box(width = 2,selectInput("month_end",label = h5("End month"),choices = list("01","02","03","04","05","06","07","08","09","10","11","12"),selected = substr(Sys.Date(),6,7)))
                                              box(width = 4,dateRangeInput(label = h4("Pick a date range"),inputId = "daterange",separator = " - ",min = "2013-01-01",
                                                                           start = Sys.Date()-3,
                                                                           end = Sys.Date()+1)),
                                            box(width = 3,selectInput("round",label = h4("Time aggregation"),choices = list("hour","day","month","year"))),#,"5 min","week",
                                            box(width = 3,downloadLink('downloadData', h4('Download')),actionButton(label= "update selection","refresh"))


                                              ,
                                              box(title = "Soil Air Precipitation Irrigation",solidHeader = TRUE,
                                                  collapsible = TRUE,status = "primary",
                                                  plotlyOutput("plotall")%>% withSpinner(), width = 12)#,
                                              #box(title = "Air",solidHeader = TRUE,
                                                  #collapsible = TRUE,status = "primary",
                                                  #plotlyOutput("plot1"), width = 12)

                                            )

                                    ),
                                    # Second tab content plotIV
                                    tabItem(tabName = "map",
                                            fluidRow(


                                              leafletOutput("map")

                                            )
                                    )#,
                                    #tabItem(tabName = "detail",
                                     #       fluidRow(
                                     #         box(width = 2,selectInput("Station", label = h4("Station"),#,multiple = T,
                                       #                                 choices = list('3_Nalls_2','7_Girlan_1','9_Gries_2','12_Unterrain_2','14_Terlan_3',
                                         #                                              '17_Kaltern_3','30_Terlan_3a','37_Algund_2','39_Plars','52_Lana_6_NEU',
                                         #                                              '70_Eppan','84_StPauls','103_Latsch_1','171_Salurn_1','172_Salurn_2','105_Latsch_3','106_Latsch_4',
                                         #                                              '125_Toell','169_Neumarkt_Stiermoos','174_Tramin_13er','176_Vill'),selected = '3_Nalls_2' )),
                                         #     box(width = 4,dateRangeInput(label = h4("Pick a date range (within the same year only)"),inputId = "daterange",separator = " - ",
                                           #                                start = Sys.Date()-3,
                                              #                             end = Sys.Date())),
                                            #  box(title = "Air",solidHeader = TRUE,
                                             #     collapsible = TRUE,status = "primary",
                                             #     plotlyOutput("plotair"), width = 12)


                                            #)
                                    #)

                                  )
                    )
)
