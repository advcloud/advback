library(plyr)
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)
require(randomForest)
library(e1071)#找支持向量机模型

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

# 画图函数
drawLine2 <-function(cdata,title,sDate=min(index(cdata)),eDate=max(index(cdata)),breaks="1 year"){
  # if(sDatemax(index(cdata))) eDate=max(index(cdata))  
  if(sDate<min(index(cdata))) sDate=min(index(cdata))
  if(eDate>max(index(cdata))) eDate=max(index(cdata))
  cdata<-na.omit(cdata)
  
  g<-ggplot(aes(x=Index, y=Value),data=fortify(cdata[,1],melt=TRUE))
  g<-g+geom_line()
  
  if(ncol(cdata)>1){ # 多条线
    g<-g+geom_line(aes(colour=Series),data=fortify(cdata[,-1],melt=TRUE))  
  }
  
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks(breaks),limits = c(sDate,eDate))
  g<-g+ylim(min(cdata$Value), max(cdata$Value))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  g
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

drawPoint2 <- function(ldata,pdata,title,sDate,eDate,breaks="1 year"){
  ldata<-na.omit(ldata)
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  
  if(is.data.frame(pdata)){
    g<-g+geom_point(aes(x=Index,y=Value,colour=op),data=pdata,size=4)
  }else{
    g<-g+geom_point(aes(x=Index,y=Value,colour=Series),data=na.omit(fortify(pdata,melt=TRUE)),size=4)  
  }
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks(breaks),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  g
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
# 合并交易信号
signal2 <- function(buy, sell){
  selldf<-data.frame(sell,op=as.character(rep("S",nrow(sell))))
  buydf<-data.frame(buy,op=as.character(rep("B",nrow(buy))))
  sdata<-rbind(buydf,selldf)                                       # 交易信号数据
  sdata[order(as.Date(row.names(sdata))),]
}

# 模拟交易
trade2 <- function(sdata,capital=100000,fixMoney=10000){ # 交易信号，总资金，每次定投资金
  amount<-0
  cash<-capital
  
  ticks<-data.frame()
  for(i in 1:nrow(sdata)){
    row<-sdata[i,]
    if(row$op=='B'){
      if(cash<fixMoney){
        print(paste(row.names(row),"No enough cash"))
        next
      }
      amount0<-floor(fixMoney/row$Value) # 本次交易量
      amount<-amount+amount0
      cash<-cash-amount0*row$Value
    }
    
    if(row$op=='S'){
      cash<-cash+amount*row$Value
      amount<-0
    }
    
    row$cash<-round(cash,2)
    row$amount<-amount
    row$asset<-round(cash+amount*row$Value,2)
    ticks<-rbind(ticks,row)
  }
  
  
  ticks$diff<-c(0,round(diff(ticks$asset),2))
  
  rise<-ticks[intersect(which(ticks$diff>0),which(ticks$op=='S')),]   # 赚钱的交易
  fall<-ticks[intersect(which(ticks$diff<0),which(ticks$op=='S')),]   # 赔钱的交易
  
  return(list(
    ticks=ticks,
    rise=rise,
    fall=fall
  ))
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


myTTR <- function(data){
  require(TTR)
  require(quantmod)
  #  names(data)<-sapply(X=names(df6),FUN=function(x) 
  #    strsplit(x,split=".",fixed=TRUE)[[1]][2])
  myATR<-ATR(HLC(data))$atr
  mySMI<-SMI(HLC(data))$SMI
  myADX<-ADX(HLC(data))$ADX
  myAroon<-aroon(HLC(data)[,-3])$oscillator
  myBBands<-BBands(HLC(data))$pctB
  myChaikin<-Delt(chaikinVolatility(HLC(data)[,-3]))[,1]
  myCLV<-EMA(CLV(HLC(data)))[,1]
  myMACD<-MACD(data[,"Close"])[,2]
  myMFI<-MFI(data[,c("High","Low","Close")],data[,"Volume"])
  mySAR<-SAR(data[,c("High","Close")])[,1]
  result<-cbind(myATR,mySMI,myADX,myAroon,myBBands,myChaikin,myCLV,myMACD,myMFI,mySAR)
  colnames(result)<-c("myATR","mySMI","myADX","myAroon","myBBands","myChaikin","myCLV","myMACD","myMFI","mySAR")
  return(result)
}
#这10个技术指标作为预测变量，用随机森林的方法精简变量
T2Signal <- function(x,a1=-0.01,a2=-a1){
  result <- ifelse(x<a1,"Sell",ifelse(x>a2,"Buy","Hold"))
  result <- factor(result)
  return(result)
}    
accuracy <- function(prediction,true)
{
  t <- table(prediction,true)
  result <- (t["Sell","Sell"]+t["Buy","Buy"])/
    (sum(t["Buy",])+sum(t["Sell",]))
  return(result)
}
ac <- function (a1,a2){
  signal6.p <- T2Signal(x=p,a1=a1,a2=a2)
  signal6.true <- T2Signal(x=na.omit(test.data)$T,a1=a1,a2=a2)
  accuracy(prediction=signal6.p,true=signal6.true)
}

Tget67 <- function(data67,p,k){
  hlc67<-HLC(data67)
  P67<-rowMeans(hlc67)
  V67<-matrix(NA,ncol=k,nrow=NROW(P67))
  for(i in 1:k){
    V67[,i]<-Next(Delt(P67,k=i),k=i)
    #Next和Delt来自quantmod包，分别为平移一个序列，计算收益率
  }
  T68<-apply(V67,1,function(x) sum(x[abs(x)>p]))
  T68<-xts(x=T68,order.by = time(data67))
  return(T68)
}

shiny <- function(input, output) {
    
  stock<-"2330.TW" # 下载台積電的股票行情数据
 # download(stock,from='2010-01-01')
  IBM<-read(stock) # 把数据加载到内存
  #stock1<-"3008.TW" # 下载台積電的股票行情数据
  #download(stock1,from='2013-01-01')
  #IBM1<-read(stock1) # 把数据加载到内存
  # 运行程序
  cdata<-IBM['2013/2016']$Close # 取收盘价
  title<-"台積電" # 图片标题
  sDate<-as.Date("2013-1-1") # 开始日期
  eDate<-as.Date("2016-1-1") # 结束日期
  vdata<-IBM['2013/2016']$Volume      # 获得交易量
  #cdata8<-IBM1['2013/2015']$Close # 取收盘价
  #title8<-"大立光" # 图片标题
  #sDate8<-as.Date("2013-1-1") # 开始日期
  #eDate8<-as.Date("2015-1-1") # 结束日期
  # ma(cdata,c(5,20,60))
  ldata<-ma(cdata,c(20))  #选择滑动平均指标
  pdata<-merge(ldata$ma20[which(ldata$Value-ldata$ma20>0)],ldata$ma20[which   (ldata$Value-ldata$ma20<0)])
  names(pdata)<-c("down","up")
  pdata<-fortify(pdata,melt=TRUE)
  pdata<-pdata[-which(is.na(pdata$Value)),]
  tdata<-Signal(cdata,pdata)
  tdata<-tdata[which(as.Date(row.names(tdata))<eDate),]
  result1<-trade(tdata,100000)
  adata<-as.xts(result1$ticks[which(result1$ticks$op=='S'),]['cash'])
  
  cdata4<-IBM['2010/2014']$Close # 取收盘价
  names(cdata4)<-"Value"   # 重置列名
  ldata4<-cbind(cdata4,minmax(cdata4))
  # 计算买入的点
  buydata<-buyPoint(ldata4)
  # 卖出信号
  selldata<-stopPoint(ldata4,buydata)
  bsdata<-merge(buydata$Value,selldata$Value)
  names(bsdata)<-c("buy","sell")
  # 合并交易信号
  sdata<-signal2(buydata,selldata)   
  # 优化条件，当股价低于前一个买入点价格时进行卖出，小于10日最低价为止损点。
  # 计算卖出的信号点
  selldata2<-sellPoint(ldata4,buydata)
  sdata2<-signal2(buydata$Value,selldata2$Value) 
  
  
  # 止损信号
  stopdata<-stopPoint(ldata4,buydata)
  
  
  bsdata2<-merge(buydata$Value,selldata2$Value,stopdata$Value)
  names(bsdata2)<-c("buy","sell","stop")
  
  #使用什么样的指标来预测
  #真实波幅ATR，随机动量指标SMI，Wells定向指标ADX，收盘价位置价值CLV，资金流向指标MFI
  #这些指标独在TTR包中找到
 # df6<-read(stock) # 把数据加载到内存
  fastMA <- SMA(Cl(IBM["2013-04-20/2013-05-31"]), n = 5)
  slowMA <- SMA(Cl(IBM["2013-03-01/2013-05-31"]), n = 30)
  signal1 <- fastMA >= slowMA
  
  x <- which(signal1["2013-05-01/2013-05-31", ])[1]
  
 
 
 
  
  output$line_out1 <- renderPlotly({
    drawPoint(ldata,pdata,title,sDate,eDate) # 画图，如图2-9所示
    
  })
  output$line_out2 <- renderPlotly({
    drawCash(ldata,adata,sDate,eDate) # 画图
   
  })
  
  
  ldata2<-ma(cdata,c(5,20)) # 选择滑动平均指标
  
  
  pdata2<-merge(ldata2$ma20[which(ldata2$ma5-ldata2$ma20>0)],ldata2$ma20[which(ldata2$ma5-
                                                                            ldata2$ma20<0)])
  names(pdata2)<-c("down","up")
  pdata2<-fortify(pdata2,melt=TRUE)
  pdata2<-pdata2[-which(is.na(pdata2$Value)),]
  #  以散点覆盖20日均线，红色点为买入持有，紫色点为卖出空仓，如图2-12所示
  
 
  tdata2<-Signal(cdata,pdata2)
  tdata2<-tdata2[which(as.Date(row.names(tdata2))<eDate),]
 
  result2<-trade(tdata2,100000)
  
    
  tail(result2$ticks,1)
  adata2<-as.xts(result2$ticks[which(result2$ticks$op=='S'),]['cash'])
 
  
  
  output$time_volumn3 <- renderPlotly({
    drawPoint(ldata2,pdata2,title,sDate,eDate) # 画图，如图2-12所示。  
   
  })
  output$travel_diagram4 <- renderPlotly({
    drawCash(ldata2,adata2,sDate,eDate)
  })
  # bus station profile ---------------------------------------------------
  output$station_map <- renderPlotly({
    drawLine2(ldata4,title,sDate,eDate,'1 month')    # 画出股价，最高价和最低价 画图
  })
  output$station_volumn <- renderPlotly({
    drawPoint2(ldata4,buydata$Value,title,sDate,eDate,'1 month')  # 计算买入的点 画图
  })
  output$station_3 <- renderPlotly({
    drawPoint2(ldata4,bsdata,title,sDate,eDate,'1 month') # 买卖信号，画图
  })
  output$station_4 <- renderPlotly({
    drawPoint2(ldata4,bsdata2,title,sDate,eDate,'1 month') ## 合并买卖信号，止损信号
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
  
    output$stationCount <- renderInfoBox({
      infoBox("站点数量(个)")
    })
    output$passengerVolumn <- renderInfoBox({
      infoBox("客流量(人次)")
    })
    output$routeLength <- renderInfoBox({
      infoBox("线路长度(公里)")
    })
    output$routeIndirect <- renderInfoBox({
      infoBox("非直线系数")
    })
    output$headway <- renderInfoBox({
      infoBox("发车间隔(分钟)")
    })
    output$vehicle <- renderInfoBox({
      infoBox("配车数(辆)")
    })
    output$volumnByLength <- renderInfoBox({
      infoBox("车公里载客人数(人/辆/公里)")
    })
    output$avgHaulDistance <- renderInfoBox({
      infoBox("平均运距(公里)")
    })
    output$turnoverTime <- renderInfoBox({
      infoBox("行程时间(分钟)")
    })
    output$peakHourRatio <- renderInfoBox({
      infoBox("高峰小时系数")
    })
    output$operationTime <- renderInfoBox({
      infoBox("运营时间")
    })
    output$operationCompany <- renderInfoBox({
      infoBox("运营公司")
    })
   
    
   
    output$get_location <- renderLeaflet({
        leaflet() %>% addTiles() %>% setView(120.79969883, 21.94491642, zoom = 12)
    })
    observeEvent(input$get_location_click, {
        
            return()
    
    })
}




