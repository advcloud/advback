library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(ggplot2)
library(data.table)
library(ggmap)

hos <- levels(as.factor(c(as.character(db$hos1Name),as.character(db$hos2Name), as.character(db$hos3Name), as.character(db$hos4Name))))


ui <- dashboardPage(
    dashboardHeader(
        title = "Advcloud map"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("液化圖", tabName = "line", icon = icon("check")),
            menuItem("社區醫療群", tabName = "station", icon = icon("circle"))
           
        ),
        div(style = "padding-left: 15px; padding-top: 40px;",
            p(class = "small", "Made with ",
              a("R", href = "http://www.r-project.org/"),
              ", ",
              a("Shiny", href = "http://shiny.rstudio.com/"),
              ", ",
              a("shinydashboard", href = "http://rstudio.github.io/shinydashboard/"),
              ", ",
              a("leaflet", href = "http://rstudio.github.io/leaflet/"),
              ", ",
              a("plotly", href = "https://plot.ly/"),
              ", ",
              a("ggplot2", href = "http://ggplot2.org/")
            ),
            p(class = "small", "Data from", 
              a("XXSPRS ")
            ),
            p(class = "small", "View", 
              a("Source code", href = "https://github.com/")
            ),
            p(class = "small", "Author", 
              a("Jack Liu")
            )
        )
    ),
    
    dashboardBody(
        tabItems(
            # bus line profile ------------------------------------------------------
            tabItem(tabName = "line",
                    fluidRow(
                      box(width = 12, title = "PL範圍:液化情形", leafletOutput("line_out1"))
                    ),
                    
                    fluidRow(
                      box(width = 12, title = "液化", dataTableOutput(outputId = 'mytable'))
                      
                    )
                    
                    
            ),
            # bus station profile ---------------------------------------------------
            tabItem(tabName = "station",
                    fluidRow(
                      box(selectInput("hospital", "Choose a hospital:", choices= hos))
                    ),
                    fluidRow(
                        box(width = 12, title = "醫療群",chartOutput("station_map", 'leaflet'))
                    ),
                    fluidRow(
                      box(width = 12, title = "社區", dataTableOutput("station_volumn"))
                    )
                    
            )
           
            # transit play ------------------------------------------------------------            
            
        )
    )
)