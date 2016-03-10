library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(ggplot2)





ui <- dashboardPage(
    dashboardHeader(
        title = "Advcloud pm2.5"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("PM 2.5 ", tabName = "line", icon = icon("check")),
            menuItem("Status", tabName = "station", icon = icon("circle")),
            menuItem("Water ", tabName = "line1", icon = icon("check")),
            menuItem("Status", tabName = "station1", icon = icon("circle"))
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
                      box(width = 12, title = "PM 2.5 Status in One of Taiwan City", leafletOutput("line_out1"))
                    ),
                    
                    fluidRow(
                      box(width = 12, title = "PM 2.5", dataTableOutput(outputId = 'mytable'))
                      
                    )
                    
                    
            ),
            # bus station profile ---------------------------------------------------
            tabItem(tabName = "station",
                    
                    fluidRow(
                              checkboxGroupInput('show_vars', '選項組 :',
                                         names(data3), selected = names(data3),inline=TRUE)
                      ),
                    fluidRow(
                      box(width = 12, title = "PM 2.5", dataTableOutput(outputId = 'mytable1'))
                      
                    )
                    
            ),
           
            # transit play ------------------------------------------------------------            
            # water line profile ------------------------------------------------------
            tabItem(tabName = "line1",
                    fluidRow(
                      box(width = 12, title = "WaterData Status in One of  City", plotOutput("line1_out1"))
                    )
                    
                  
                    
                    
            ),
            # water station profile ---------------------------------------------------
            tabItem(tabName = "station1",
                    
                    fluidRow(
                      checkboxGroupInput('show_vars1', '選項組 :',
                                         names(WaterDataFrame), selected = names(WaterDataFrame),inline=TRUE)
                    ),
                    fluidRow(
                      box(width = 12, title = "WaterData Status in One of  City", dataTableOutput(outputId = 'mytable2'))
                      
                    )
                    
            )
        )
    )
)