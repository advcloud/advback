library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(ggplot2)





ui <- dashboardPage(
    dashboardHeader(
        title = "Advcloud wind"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("颱風移動路徑的可視化 ", tabName = "line", icon = icon("check")),
            menuItem("Status", tabName = "station", icon = icon("circle")),
            menuItem("연도 별로 태풍 경로 시각화 ", tabName = "line1", icon = icon("check"))
           
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
                      box(width = 12, title = "颱風移動路徑的可視化 ", plotOutput("line_out1"))
                    )
                    
                   
                    
                    
            ),
            # bus station profile ---------------------------------------------------
            tabItem(tabName = "station",
                    
                    fluidRow(
                              checkboxGroupInput('show_vars', '選項組 :',
                                         names(WP.basin), selected = names(WP.basin),inline=TRUE)
                      ),
                    fluidRow(
                      box(width = 12, title = "연도 별로 태풍 경로 시각화", dataTableOutput(outputId = 'mytable1'))
                      
                    )
                    
            ),
           
            # transit play ------------------------------------------------------------            
            # water line profile ------------------------------------------------------
            tabItem(tabName = "line1",
                    fluidRow(
                      box(width = 12, title = "연도 별로 태풍 경로 시각화", plotOutput("line1_out1"))
                    )
                    
                  
                    
                    
            )
            # water station profile ---------------------------------------------------
           
        )
    )
)