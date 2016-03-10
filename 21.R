library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(ggplot2)
library(data.table)
library(ggmap)


ui <- dashboardPage(
    dashboardHeader(
        title = "Advcloud ubike"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("捷運市政府站", tabName = "line", icon = icon("check")),
            menuItem("趨勢線", tabName = "bus", icon = icon("circle")),
            menuItem("散佈圖", tabName = "od", icon = icon("bus")),
            menuItem("場站標示", tabName = "map", icon = icon("exchange")),
            menuItem("分群", tabName = "kmeans", icon = icon("check"))
           
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
            #  line profile ------------------------------------------------------
            tabItem(tabName = "line",
                    fluidRow(
                      box(width = 12, title = "市府站車輛數基本數據", plotOutput("line_out1"))
                    ),
                    fluidRow(
                      box(width = 12, title = "場站有車率與晴雨關係",tableOutput( 'line_table1'))
                    ),
                    fluidRow(
                      box(width = 12, title = "Stack plot (堆疊圖)", plotOutput("line_out2"))
                    ),
                    
                    fluidRow(
                      box(width = 12, title = "Dodge plot", plotOutput("line_out3"))
                    ),
                    fluidRow(
                      box(width = 12, title = "Fill plot", plotOutput("line_out4"))
                    ),
                    fluidRow(
                      box(width = 12, title = "Facet panels in a grid", plotOutput("line_out5"))
                    ),
                    fluidRow(
                      box(width = 12, title = "Facet panels in a grid", plotOutput("line_out6"))
                    )
                      
                   
                    
                    
            ),
            # bus station profile ---------------------------------------------------
            tabItem(tabName = "bus",
                   
                    fluidRow(
                      box(width = 12, title = "Pyramid", plotOutput("bus_out1"))
                    ),
                    fluidRow(
                      box(width = 12, title = "Line chart", plotOutput("bus_out2"))
                    ),
                    fluidRow(
                      box(width = 12, title = "Smoothing (平滑曲線)", plotOutput("bus_out3"))
                    ),
                    fluidRow(
                      box(width = 12, title = "Polar coordinates (極座標化)", plotOutput("bus_out4"))
                    ),
                    fluidRow(
                      box(width = 12, title = "熱點圖", plotOutput("bus_out5"))
                    ),
                    fluidRow(
                      box(width = 12, title = "平行座標圖", plotOutput("bus_out6"))
                    ),
                    fluidRow(
                      box(width = 12, title = "平行座標圖", plotOutput("bus_out7"))
                    )
                   
                    
            ),
           
            # transitod play ------------------------------------------------------------            
            tabItem(tabName = "od",
                    fluidRow(
                      box(width = 12, title = "場站 有車率 與 使用率 的探索", tableOutput("od_table1"))
                    ),
                    
                    fluidRow(
                      box(width = 12, title = "散佈圖", plotOutput("od_out1"))
                    ),
                    fluidRow(
                      box(width = 12, title = "定義象限", plotOutput("od_out2"))
                    ),
                    fluidRow(
                      box(width = 12, title = "顏色分類", plotOutput("od_out3"))
                    ),
                    fluidRow(
                      box(width = 12, title = "Facet grid, color, and shape", plotOutput("od_out4"))
                    ),
                    fluidRow(
                      box(width = 12, title = "Facet grid, color, and shape", plotOutput("od_out5"))
                    ),
                    fluidRow(
                      box(width = 12, title = "與鄰近場站的關係", tableOutput("od_table2"))
                    )
                    
                    
            ),
            # map play ------------------------------------------------------------            
            tabItem(tabName = "map",
                    
                    fluidRow(
                      box(width = 12, title = "導入google map作為底圖將場站位置標示出來", plotOutput("map_out1"))
                    ),
                    fluidRow(
                      box(width = 12, title = "改變場站標示的大小", plotOutput("map_out2"))
                    ),
                    fluidRow(
                      box(width = 12, title = "熱點圖進階應用", plotOutput("map_out3"))
                    ),
                    fluidRow(
                      box(width = 12, title = "熱點圖", plotOutput("map_out4"))
                    ),
                    fluidRow(
                      box(width = 12, title = "heatmap 排序", tableOutput("map_table1"))
                    ),
                    fluidRow(
                      box(width = 12, title = "heatmap", tableOutput("map_table2"))
                    ),
                    fluidRow(
                      box(width = 12, title = "heatmap", plotOutput("map_out5"))
                    ),
                    fluidRow(
                      box(width = 12, title = "對時間做排序", plotOutput("map_out6"))
                    ),
                    fluidRow(
                      box(width = 12, title = "對時間做排序", plotOutput("map_out7"))
                    ),
                    fluidRow(
                      box(width = 12, title = "試著對 使用率 進行排序", plotOutput("map_out8"))
                    )
                    
            ),
            # kmean play ------------------------------------------------------------            
            tabItem(tabName = "kmeans",
                    
                    fluidRow(
                      box(width = 12, title = "平行座標圖進階應用", plotOutput("kmeans_out1"))
                    ),
                    
                    fluidRow(
                      box(width = 12, title = "分3群", tableOutput("kmeans_table1"))
                    )
            )
        )
    )
)