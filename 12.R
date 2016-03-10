library(httr)
library(RCurl)
library(XML)
library(bitops)
library(plyr)
library(ggplot2)
library(leaflet)
library(ggmap)

### Top 10
p <- ggplot(head(airport.krjp,10), aes(x=IATA, y=Freq))
p + geom_bar(stat="identity", fill="#2980B9") + 
  geom_text(aes(label=Freq), vjust=1.5, colour="white", size=4) +
  xlab("") + ylab("Counts") + 
  ggtitle("Arriving flight routes(KR, JP only)") + 
  theme_bw()

shiny <- function(input, output) {
    
  
  
  output$line_out1 <- renderPlot({
    
    
    ## 지도 그리기
    map <- ggmap(get_googlemap(center = c(lon=134, lat=36), 
                               zoom=5,maptype='roadmap', 
                               color='bw', scale=2), 
                 extent='device')
    
    map + geom_point(data=airport.krjp,aes(x=longitude,y=lantitude,size=Freq),
                     colour = 'gray10',alpha=0.3) +
      geom_line(data=data.line,aes(x=longitude,y=lantitude, group=group),
                size=0.2,alpha=.1,color='#816960') +
      scale_size(range=c(0,15)) + theme(legend.position="none")
    
    ###
    
    
  })
 
 
 
  # bus station profile ---------------------------------------------------
  diamonds2 = airport.krjp[sample(nrow(airport.krjp), ), ]
  output$mytable1 <- renderDataTable({
    diamonds2[, input$show_vars, drop = FALSE]
  })
  
  output$line1_out1 <- renderPlot({
    
    map <- get_map(location = 'Europe', zoom = 4)
    mapPoints <- ggmap(map) +
      geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportD, alpha = .5)
    mapPointsLegend <- mapPoints +
      scale_size_area(breaks = sqrt(c(1, 5, 10, 50, 100, 500)), labels = c(1, 5, 10, 50, 100, 500), name = "departing routes")
    mapPointsLegend
    
    mapPointsDA <- ggmap(map) +
      geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportDA, alpha = .5)
    # adjust the legend
    mapPointsLegendDA <- mapPointsDA + 
      # scale_area()
      scale_size_area(breaks = sqrt(c(1, 5, 10, 50, 100, 500)), labels = c(1, 5, 10, 50, 100, 500), name = "routes1")
    # panels according to type (departure/arrival)
    mapPointsFacetsDA <- mapPointsLegendDA +
      facet_grid(. ~ type)
    # plot the map
    mapPointsFacetsDA
    
  })
  
  
  
  # bus station profile ---------------------------------------------------
  diamonds3 = routes1[sample(nrow(routes1), ), ]
  output$mytable2 <- renderDataTable({
    diamonds3[, input$show_vars1, drop = FALSE]
  })
  
 
}




