library(shiny)
library(ggvis)
library(ggmap)
library(dplyr)
library(RgoogleMaps)
library(ggplot2)
library(RColorBrewer)

library(shinydashboard)

ui <- shinyUI(fluidPage(
  titlePanel("桃園竊盜分析"),
  h3("Datarange:104/11~105/1", style = "color:red"),
#  downloadButton('downloadData', 'Download'),
  br(),
  span("last update:105/3/9", style = "color:blue"),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("竊盜熱點",
                         fluidRow(     
                           column(3,
                                  selectInput("map_type", label = h4("竊盜種類"),c("All","機車竊盜","住宅竊盜","汽車竊盜")),
                                  selectInput("map_breau", label = h4("分局"),
                                              c("All","桃園分局","中壢分局","楊梅分局","大園分局","大溪分局","平鎮分局","八德分局","龜山分局","蘆竹分局","龍潭分局")),                                   
                                  selectInput("map_year", label = h4("年月"),c(10411,10412,10501)),      
                                  sliderInput("map_zoom", label = h4("縮放"),min = 11, max = 14, value = 11),
                                  span("Design by adv", style = "color:darkorange")
                           ),
                           column(6,
                                  h3("竊盜密度圖"),
                                  plotOutput("map",width = "800px",height = "800px")
                           )
                         )),#map_panel
                tabPanel("轄區分析",
                         fluidRow(
                           column(3,
                                  selectInput("anb_year", label = h4("年度"),c(10411,10412,10501)),      
                                  span("Design by adv", style = "color:darkorange")
                                  
                           ),
                           column(3,
                                  h3("轄區竊盜件數堆疊直方圖"),
                                  ggvisOutput("breau_plot")
                           ) 
                         )
                ),#Analysis_breau_panel
                tabPanel("資料檢視",
                         fluidRow(
                           column(3,
                                  selectInput("data_type", label = h4("竊盜種類"),c("All","住宅竊盜","機車竊盜","汽車竊盜")),
                                  selectInput("data_breau", label = h4("分局"),
                                              c("All","桃園分局","中壢分局","楊梅分局","大園分局","大溪分局","平鎮分局","八德分局","龜山分局","蘆竹分局","龍潭分局")),
                                  selectInput("data_year", label = h4("年度"),c(10411,10412,10501)),      
                                  span("Design by adv", style = "color:darkorange")
                           ),
                           column(6,
                                  h3("資料檢視"),
                                  tableOutput("Data")     
                           )
                         )
                )#Data_panel
    )
  )#mainPanel
))