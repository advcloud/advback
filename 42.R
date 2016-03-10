library(plyr)
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)
library(leaflet)
library(dplyr)
library(rCharts)

download<-function(stock,from="2010-01-01"){ # 下载数据并保存到本地
  df<-getSymbols(stock,from=from,env=environment(),auto.assign=FALSE)  # 下载数据
  names(df)<-c("Open","High","Low","Close","Volume","Adjusted")
  write.zoo(df,file=paste(stock,".csv",sep=""),sep=",",quote=FALSE) # 保存到本地文件
}

read<-function(stock){ # 从本地文件读数据
  as.xts(read.zoo(file=paste(stock,".csv",sep=""),header = TRUE,sep=",",                      format="%Y-%m-%d"))
}

ma<-function(cdata,mas=c(5,20,60)){ 
  ldata<-cdata
  for(m in mas){    ldata<-merge(ldata,SMA(cdata,m))          }
  ldata<-na.locf(ldata, fromLast=TRUE)
  names(ldata)<-c('Value',paste('ma',mas,sep=''))
  return(ldata)
}

drawLine<-function(ldata,titie="Stock_MA",sDate=min(index(ldata)),eDate=  max(index(ldata)),out=FALSE){ 
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks ("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  if(out) ggsave(g,file=paste(titie,".png",sep=""))else g
}

drawPoint<-function(ldata,pdata,titie,sDate,eDate){
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  g<-g+geom_point(aes(x=Index,y=Value,colour=Series),data=fortify(pdata,melt=TRUE))
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle("台積電")
  g
}

Signal<-function(cdata,pdata){ # 交易信号
  tmp<-''
  tdata<-ddply(pdata[order(pdata$Index),],.(Index,Series),function(row){
    if(row$Series==tmp) return(NULL)
    tmp<<-row$Series
  })
  tdata<-data.frame(cdata[tdata$Index],op=ifelse(tdata$Series=='down','B','S'))
  names(tdata)<-c("Value","op")
  return(tdata)
}

trade<-function(tdata,capital=100000,position=1,fee=0.00003){   
  # 交易信号，本金，持仓比例，手续费比例
  amount<-0 # 持股数量
  cash<-capital # 现金
  
  ticks<-data.frame()
  for(i in 1:nrow(tdata)){
    row<-tdata[i,]
    if(row$op=='B'){
      amount<-floor(cash/row$Value)
      cash<-cash-amount*row$Value
    }
    
    if(row$op=='S'){
      cash<-cash+amount*row$Value
      amount<-0
    }
    
    row$cash<-cash # 现金
    row$amount<-amount # 持股数量
    row$asset<-cash+amount*row$Value # 资产总值
    ticks<-rbind(ticks,row)
  }
  
  ticks$diff<-c(0,diff(ticks$asset)) # 资产总值差
  
  # 赚钱的操作
  rise<-ticks[c(which(ticks$diff>0)-1,which(ticks$diff>0)),]
  rise<-rise[order(row.names(rise)),]
  
  # 赔钱的操作
  fall<-ticks[c(which(ticks$diff<0)-1,which(ticks$diff<0)),]
  fall<-fall[order(row.names(fall)),]
  
  return(list(
    ticks=ticks,
    rise=rise,
    fall=fall
  ))
}

drawCash<-function(ldata,adata,sDate,eDate){
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(x=as.Date(Index), y=Value,colour=Series),data=fortify
                 (adata,melt=TRUE))
  g<-g+facet_grid(Series ~ .,scales = "free_y")
  g<-g+scale_y_continuous(labels = dollar)
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),
                    limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle("台積電")
  g
}

drawRange<-function(ldata,plan,titie="Stock_2014",sDate=min(index(ldata)),
                    eDate=max(index(ldata)),out=FALSE){
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  g<-g+geom_rect(aes(NULL, NULL,xmin=start,xmax=end,fill=plan),ymin = yrng[1], 
                 ymax = yrng[2],data=plan)
  g<-g+scale_fill_manual(values =alpha(c("blue", "red"), 0.2))
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks
                    ("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  
  if(out) ggsave(g,file=paste(titie,".png",sep=""))
  else g
}



minmax <- function(data,max=20,min=10){
  d1<-na.locf(data,fromLast=TRUE)
  d2<-merge(d1,min=runMin(d1,min),max=runMax(d1,max))
  return(d2[,-1])
}

buyPoint <- function(ldata){   
  idx<-which(ldata$Value == ldata$max)
  return(ldata[idx,])                                  
}



# 计算卖出的信号点
stopPoint <- function(ldata,buydata){  
  idx<-which(ldata$Value == ldata$min)
  idx<-idx[which(c(0,diff(idx))!=1)]   # 第一点用0表示
  
  selldata<-ldata[idx,]               # 所有低于最小值的点  
  idx2<-sapply(index(buydata),function(e){  # 买后的卖点
    head(which(index(selldata)>e),1)
  })
  
  return(selldata[unique(idx2),])
} 



# 计算卖出的信号点
sellPoint <- function(ldata,buydata){
  
  arr<-c()
  for(i in 1:nrow(buydata)){
    
    if(i>1){ # 跳转第一个点
      date<-index(buydata[i,])#;print(date)      
      
      # 价格 小于 上一次的买入的价格就卖出
      last<-as.vector(buydata[i-1,]$Value) # 上一次买入的价格
      lst<-ldata[paste(date,"/",sep="")]$Value      
      idx<-head(which(lst < last),1)
      
      if(length(idx)>0){        
        arr<-rbind(arr,index(lst[idx]))
      }
    }
  }
  selldata<-ldata[as.Date(unique(arr)),]
  
  # 过滤多余的卖出点
  bsdata<-merge(buydata$Value,selldata$Value)
  names(bsdata)<-c("buy","Value")
  idx1<-which(!is.na(bsdata$Value))
  idx2<-idx1[which(c(0,diff(idx1))==1)]
  bsdata$Value[idx2]<-NA
  return(bsdata$Value[which(!is.na(bsdata$Value))])
  
}




shiny <- function(input, output) {
    
  f_map <-  read.delim("MAP.csv",stringsAsFactors=FALSE)
 
  
  output$line_out1 <- renderLeaflet({
   
    pal <- colorBin(palette=c("green","pink","red","purple"),domain=f_map$Pl,bins=c(0,5,10,15,70),pretty=TRUE,na.color="#808080",alpha=F)
    local_map <- f_map$Location %>% unique()
    
    
    
    m <- leaflet(f_map) %>% addTiles(  )
    m %>%
      addCircleMarkers(   lat = ~ Lat , lng = ~ Lon , stroke=FALSE , color=~pal(Pl), group=~Location) %>%
      addLayersControl( overlayGroups=local_map)%>%
      addLegend(position='topright' , pal=pal, values=~Pl)
    
  })
 
 
  output$mytable <- renderDataTable({
    f_map
  })
  # bus station profile ---------------------------------------------------
  output$station_map <- renderMap({
    plotMap(filterData(input$hospital, select=c("X", "clinicName", "clinicAddr", "lon", "lat", "groupName", "groupRef")))
  })
  output$station_volumn <-  renderDataTable({
    filterData(input$hospital, select=c("clinicName", "clinicAddr", "groupName"))
  })
  output$station_3 <- renderPlotly({
    
  })
  output$station_4 <- renderPlotly({
    
  })
  
 # transit OD ------------------------------------------------------------
 
 output$transit_O <- renderPlotly({
 
  })
  output$transit_D <- renderPlotly({
  #  varImpPlot(rf@fitted.model)#查看各个变量的重要性
    #buildModel()建立模型的第一个参数是x已经构建好的式子，training.per训练数据集，
  })
  output$transit_OD <- renderPlotly({
   
    
  })
  
    
   
    output$get_location <- renderLeaflet({
        leaflet() %>% addTiles() %>% setView(120.79969883, 21.94491642, zoom = 12)
    })
    observeEvent(input$get_location_click, {
        
            return()
    
    })
}




