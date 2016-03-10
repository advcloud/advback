library(httr)
library(RCurl)
library(XML)
library(bitops)
library(plyr)
library(ggplot2)
library(leaflet)
library(ggmap)

df <- data3

df <- df [,c('SiteName','TWD97Lon','TWD97Lat','PM2.5','PublishTime','Status','AreaName','County.x')]

df$lan <- as.numeric(df$TWD97Lat)
df$lon <- as.numeric(df$TWD97Lon)
df$pm <- as.numeric(as.character(df$PM2.5))

df1 <- df [,c('SiteName','pm','PublishTime','Status','County.x','AreaName')]

df2 <- df1[ order(-df1$pm),]

pal <- colorBin(palette=c("green","pink","red","purple"),domain=df$pm,bins=c(0,12,25,50,500),pretty=TRUE,na.color="#808080",alpha=F)
local_map <- df$AreaName %>% unique()

shiny <- function(input, output) {
    
  
  
  output$line_out1 <- renderLeaflet({
    
    
    m <- leaflet(df) %>% addTiles(  )
    m %>%
      addCircleMarkers(   lat = ~ lan , lng = ~ lon , stroke=FALSE , color=~pal(pm), group=~SiteName) %>%
      addLayersControl( overlayGroups=local_map)%>%
      addLegend(position='topright' , pal=pal, values=~pm)
    
    
  })
 
 
  output$mytable <- renderDataTable({
  df2
  })
  # bus station profile ---------------------------------------------------
  diamonds2 = data3[sample(nrow(data3), ), ]
  output$mytable1 <- renderDataTable({
    diamonds2[, input$show_vars, drop = FALSE]
  })
  
  output$line1_out1 <- renderPlot({
    
    
    TaipeiMap = get_map(location = c(121.43,24.93,121.62,25.19), zoom = 11, maptype = 'roadmap')
    TaipeiMapO = ggmap(TaipeiMap)+ geom_point(data=subset(WaterDataFrame,qua_cntu>=0), aes(x=longitude, y=latitude,color=qua_cntu,size=3.5))+ 
      scale_color_continuous(low = "yellow",high = "red")+ guides(size=FALSE)
    TaipeiMapO
    
    
  })
  
  
  
  # bus station profile ---------------------------------------------------
  diamonds3 = WaterDataFrame[sample(nrow(WaterDataFrame), ), ]
  output$mytable2 <- renderDataTable({
    diamonds3[, input$show_vars1, drop = FALSE]
  })
  
 
}




