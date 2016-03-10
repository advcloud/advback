library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(ggplot2)





ui <- dashboardPage(
    dashboardHeader(
        title = "Advcloud traffic"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("統計 ", tabName = "line", icon = icon("check")),
            menuItem("月份趨勢", tabName = "station", icon = icon("circle"))
            
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
                      box(width = 12, title = "104年交通違規舉發件數", plotOutput("line_out1"))
                    ),
                    fluidRow(
                      box(width = 12, title = "104年交通違規舉發車種別件數", plotOutput("line_out2"))
                    ),
                    fluidRow(
                      box(width = 12, title = "104年交通違規舉發方式別件數", plotOutput("line_out3"))
                    ),
                    fluidRow(
                      box(width = 12, title = "104年交通違規盒鬚圖", plotOutput("line_out4"))
                    )
                    
                    
            ),
            # bus station profile ---------------------------------------------------
            tabItem(tabName = "station",
                    
                    fluidRow(
                      box(width = 12, title = "交通違規舉發", plotOutput("bus_out1"))
                    ),
                    fluidRow(
                      box(width = 12, title = "趨勢線", plotOutput("bus_out2"))
                    ),
                    fluidRow(
                      box(width = 12, title = "104年交通違規車種趨勢線", plotOutput("bus_out3"))
                    ),
                    fluidRow(
                      box(width = 12, title = "104年交通違規舉發方式趨勢線", plotOutput("bus_out4"))
                      
                    ),
                    fluidRow(
                      box(width = 12, title = "交通違規熱度圖", plotOutput("bus_out5"))
                      
                    )
                    
            )
           
            # transit play ------------------------------------------------------------            
            # water line profile ------------------------------------------------------
            
        )
    )
)