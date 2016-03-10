library(httr)
library(RCurl)
library(XML)
library(bitops)
library(plyr)
library(ggplot2)
library(leaflet)
library(ggmap)

### Top 10


shiny <- function(input, output) {
    
  
  
  output$line_out1 <- renderPlot({
    
    
    map <- ggmap(get_googlemap(center = c(lon=145, lat=40), 
                               zoom=3, maptype="terrain", 
                               ,color="bw", scale=2), 
                 extent="device")
    
    # 태풍이동경로 1999 - 2010
    map + geom_path(data = substorms, 
                    aes(x = Longitude, y = Latitude,group = ID, colour = Wind.WMO.), 
                    alpha = 0.5, size = 0.8) + 
      labs(x = "", y = "", colour = "Wind \n(m/sec)",
           title = "Typhoon Trajectories 1999 - 2010") +
      theme(panel.background = element_rect(fill = "gray10", colour = "gray30"), 
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    
    
    
    
  })
 
 
 
  # bus station profile ---------------------------------------------------
  diamonds2 = WP.basin[sample(nrow(WP.basin), ), ]
  output$mytable1 <- renderDataTable({
    diamonds2[, input$show_vars, drop = FALSE]
  })
  
  output$line1_out1 <- renderPlot({
    
    map + 
      geom_path(
        data = substorms, 
        aes(x = Longitude, y = Latitude,group = ID, colour = Wind.WMO.), 
        alpha = 0.5, size = 0.8
      ) + 
      labs(
        x = "", y = "", colour = "Wind \n(m/sec)",
        title = "Typoon Trajectories by Year (1999 - 2010)"
      ) + 
      facet_wrap(~Season) +  
      # facet_grid(~Month) + 
      theme(
        panel.background = element_rect(fill = "gray10", colour = "gray30"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
      )
    
  })
  
  
 
  
 
}




