library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(ggplot2)
library(data.table)
library(ggmap)

ui <- dashboardPage(
    dashboardHeader(
        title = "Advcloud stock"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("均線模型", tabName = "line", icon = icon("check")),
            menuItem("追漲殺跌模型", tabName = "station", icon = icon("circle"))
           
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
                      box(width = 12, title = "一條20日均線的交易策略模型", plotlyOutput("line_out1"))
                    ),
                    
                    
                    fluidRow(
                        box(width = 12, title = "資金曲線", plotlyOutput("line_out2"))
                    ),
                    fluidRow(
                        box(width = 12, title = "5日均線和20日均線交叉來進行交易", plotlyOutput("time_volumn3"))
                    ),
                    fluidRow(
                        box(width = 12, title = "資金曲線", plotlyOutput("travel_diagram4"))
                    )
            ),
            # bus station profile ---------------------------------------------------
            tabItem(tabName = "station",
                    fluidRow(
                        box(width = 12, title = "藍色線為最近20日最高價，紅色線為最近10日最低價", plotlyOutput("station_map"))
                    ),
                    fluidRow(
                      box(width = 12, title = "計算買入的點", plotlyOutput("station_volumn"))
                    ),
                    fluidRow(
                      box(width = 12, title = "買賣信號", plotlyOutput("station_3"))
                    ),
                    fluidRow(
                        box(width = 12, title = "合併買賣信號，止損信號", plotlyOutput("station_4"))
                    )
            )
           
            # transit play ------------------------------------------------------------            
            
        )
    )
)