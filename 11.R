library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(ggplot2)





ui <- dashboardPage(
    dashboardHeader(
        title = "Advcloud airports"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("공항정보 불러오기  ", tabName = "line", icon = icon("check")),
            menuItem("Status", tabName = "station", icon = icon("circle")),
            menuItem("Europe ", tabName = "line1", icon = icon("check")),
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
                      box(width = 12, title = "공항정보 불러오기 ", plotOutput("line_out1"))
                    )
                    
                   
                    
                    
            ),
            # bus station profile ---------------------------------------------------
            tabItem(tabName = "station",
                    
                    fluidRow(
                              checkboxGroupInput('show_vars', '選項組 :',
                                         names(airport.krjp), selected = names(airport.krjp),inline=TRUE)
                      ),
                    fluidRow(
                      box(width = 12, title = "Flights", dataTableOutput(outputId = 'mytable1'))
                      
                    )
                    
            ),
           
            # transit play ------------------------------------------------------------            
            # water line profile ------------------------------------------------------
            tabItem(tabName = "line1",
                    fluidRow(
                      box(width = 12, title = "Europe", plotOutput("line1_out1"))
                    )
                    
                  
                    
                    
            ),
            # water station profile ---------------------------------------------------
            tabItem(tabName = "station1",
                    
                    fluidRow(
                      checkboxGroupInput('show_vars1', '選項組 :',
                                         names(routes1), selected = names(routes1),inline=TRUE)
                    ),
                    fluidRow(
                      box(width = 12, title = "Flights", dataTableOutput(outputId = 'mytable2'))
                      
                    )
                    
            )
        )
    )
)