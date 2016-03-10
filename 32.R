library(shiny)
library(ggvis)
library(ggmap)
library(dplyr)
library(RgoogleMaps)
library(ggplot2)
library(RColorBrewer)

shiny  <- (function(input, output) {
  
 # output$downloadData <- downloadHandler(
#    filename = function() { paste('Taoyuan_crime', '.csv') },
#    content = function(file) {
#      write.csv(data, file,row.names = F,fileEncoding = "utf8")
 #   })#下載資料功能
  
  zoom <- reactive({
    input$map_zoom
  })#地圖縮放
  
  
  output$map <- renderPlot({
    ym <- as.numeric(input$map_year) #轉換為numeric
    y1 <- round(ym / 100) #取前3個數字
    m1 <- ym %% 100 #取後2個數字
    if(input$map_breau == "All" & input$map_type == "All"){
      fdata <- data %>%
        filter(year == y1,month == m1)
    }else if(input$map_breau == "All" & input$map_type != "All"){
      fdata <- data %>%
        filter(year == y1,month == m1,type == input$map_type)
    }else if (input$map_breau != "All" & input$map_type == "All"){
      fdata <- data %>%
        filter(year == y1,month == m1,breau == input$map_breau)
    }else{
      fdata <- data %>%
        filter(year == y1,month == m1,breau == input$map_breau,type == input$map_type)
    }#過濾地圖資料
    
    getmap <- get_googlemap(center = c(lon = mean(fdata$lon),lat = mean(fdata$lat)),zoom=zoom(),maptype = "roadmap")
    #根據點位中心取得地圖
    ggmap(getmap,extent = "device",ylab = "lat",xlab = "lon",maprange=FALSE) +
      geom_point(data = fdata,colour = "darkred", pch=16, cex= 2.5,alpha = 1) +
      stat_density2d(data = fdata, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),size = 0.01, geom = 'polygon')+
      scale_fill_gradient(low = "green", high = "red") +
      scale_alpha(range = c(0.05, 0.15))  +
      theme(legend.position = "none") 
  })#根據取得的地圖畫出地圖內的點跟密度
  
  breau_plot <- reactive({
    ym <- as.numeric(input$anb_year)
    y1 <- round(ym / 100)
    m1 <- ym %% 100
    data %>% 
      filter(year == y1,month == m1) %>% 
      group_by(breau,type) %>%
      summarise(count = n()) %>%
      ggvis(~breau,~count) %>%
      layer_bars(fill = ~type,width = 0.5) 
  })#堆疊直方圖
  breau_plot %>% bind_shiny("breau_plot")
  
  output$Data <- renderTable({
    ym <- as.numeric(input$data_year)
    y1 <- round(ym / 100)
    m1 <- ym %% 100
    if(input$data_breau != "All" & input$data_type != "All"){
      data %>% filter(year == y1,month == m1,breau == input$data_breau,type == input$data_type)
    }else if(input$data_breau !="All" & input$data_type == "All"){
      data %>% filter(year == y1,month == m1,breau == input$data_breau)
    }else if(input$data_breau =="All" & input$data_type != "All"){
      data %>% filter(year == y1,month == m1,type == input$data_type)
    }else{
      data %>% filter(year == y1,month == m1)
    }
  })#資料檢視過濾
})