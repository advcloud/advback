library(leaflet)
library(knitr) 
library(ggplot2)
library(GGally)
library(dplyr)
library(geosphere)
library(ggmap)
library(reshape2)
library(ggdendro)

ubiket <- fread("ubike.csv", 
                showProgress = interactive(), data.table = FALSE)


ubike1 <- filter(ubiket, sno==1) %>%
  mutate(sbi.range=max.sbi-min.sbi) %>%
  mutate(is.rushhours=cut(hour, breaks=c(0, 8, 10, 17, 20, 24), 
                          labels = c(0,1,0,1,0), right=FALSE)) %>%
  mutate(is.weekday=ifelse(strftime(date, "%u") < 6, 1, 0))

tab1 <- filter(ubike1, is.rushhours==1, is.weekday==1) %>% 
  group_by(tot) %>%
  summarise(min(sbi.range), mean(sbi.range), max(sbi.range))

df1 <- group_by(ubike1, date, hour) %>%
  summarise(rate.sbi=mean(avg.sbi)/tot) %>%
  group_by(hour) %>%
  summarise(rate.sbi=mean(rate.sbi))

thm <- function() {
  theme_gray(base_family = "STHeiti") + 
    theme(text=element_text(size=18))
}

df2 <- filter(ubiket, sno==1) %>% 
  mutate(is.rain=rainfall>1) %>%
  mutate(is.rain=factor(is.rain, levels=c(FALSE, TRUE), 
                        labels = c("晴天","雨天"))) %>%
  select(date, hour, tot, avg.sbi, avg.bemp, temp, is.rain) %>%
  group_by(date, hour, is.rain) %>%
  summarise(rate.sbi=mean(avg.sbi)/tot) %>%
  group_by(hour, is.rain) %>%
  summarise(rate.sbi=mean(rate.sbi))

df3 <- filter(ubike1, sno==1) %>% 
  mutate(is.rain=rainfall>1) %>%
  mutate(is.rain=factor(is.rain, levels=c(FALSE, TRUE), 
                        labels = c("晴天","雨天"))) %>%
  mutate(is.weekday=strftime(date, "%u")<6) %>%
  mutate(is.weekday=factor(is.weekday, levels=c(FALSE, TRUE),
                           labels=c("平日","假日"))) %>%
  mutate(is.rushhours=cut(hour, breaks=c(0, 4, 7, 24), right=FALSE)) %>%
  group_by(date, is.weekday, is.rushhours, hour, is.rain) %>%
  summarise(rate.sbi=mean(avg.sbi)/tot, rate.used=mean(max.sbi-min.sbi)/tot)

df4 <- filter(df3, hour>6 & hour < 10)

tmp <- group_by(ubiket, sno, sna, sarea, lat, lng) %>% distinct 
dist <- round(distm(x=tmp[, c("lng","lat")])[,1])
df5 <- tmp %>% select(sno, sna, sarea, lat, lng) %>% 
  cbind(dist) %>% arrange(dist) %>% top_n(10, wt = -dist)

tmp1 <- filter(ubiket, sno%in%df5$sno) %>%
  mutate(is.rain=rainfall>1) %>%
  mutate(is.rain=factor(is.rain, levels=c(FALSE, TRUE), 
                        labels = c("晴天","雨天"))) %>%
  mutate(is.weekday=strftime(date, "%u")<6) %>%
  mutate(is.weekday=factor(is.weekday, levels=c(FALSE, TRUE),
                           labels=c("平日","假日"))) %>%
  mutate(is.rushhours=cut(hour, breaks=c(0, 4, 7, 24), right=FALSE)) %>%
  group_by(date, sno, sna, is.weekday, is.rushhours, is.rain, hour, tot) %>%
  summarise(rate.sbi=mean(avg.sbi)/tot, rate.used=mean(max.sbi-min.sbi)/tot)

df6 <- tmp1 %>%
  filter(is.weekday=="平日", is.rain=="晴天") %>%
  group_by(sno, sna, sna, hour) %>%
  summarise(rate.sbi=mean(rate.sbi), rate.used=mean(rate.used))


dat <- dcast(df6, sna~hour, value.var="rate.used")
rownames(dat) <- dat[,1]
dat <- dat[,-1]
hc.sna <- hclust(dist(dat))
hc.hour <- hclust(dist(t(dat)))

ggdendrogram(hc.sna, rotate = TRUE) + thm() + labs(x="", y="")
sna.order <- data.frame(order=1:10, sna=hc.sna$labels[hc.sna$order])
df7 <- df6
df7$sna <- factor(df7$sna, levels=(sna.order[,2]))

df8 <- df6
df8$sna <- factor(df8$sna, levels = hc.sna$labels[hc.sna$order])
df8$hour <- factor(df8$hour, levels = hc.hour$labels[hc.hour$order])

tmp2 <- filter(tmp1, is.weekday=="平日", is.rain=="晴天", hour>6 & hour<22) %>%
  group_by(sno, sna, tot) %>%
  summarise(rate.sbi=mean(rate.sbi), rate.used=mean(rate.used))
km <- kmeans(tmp2[,3:5], 3)
km
df9 <- group_by(tmp2) %>% 
  transmute(sna, tot, rate.sbi, rate.used,
            group=factor(km$cluster))  %>% 
  arrange(group)



shiny <- function(input, output) {
    
 
 
  
  output$line_out1 <- renderPlot({
   
    
    ggplot(df1, aes(x=hour, y=rate.sbi)) +
      geom_bar(stat="identity") + 
      ggtitle("捷運市政府站") +
      labs(x="時間", y="有車率") +
      thm() +
      theme(legend.title=element_blank()) + 
      scale_y_continuous()
    
  })
 
 
  output$line_table1 <- function(){
    paste( kable(df2[17:22,], format="html", digits=3, align="c"))
  }
  
  
  
  output$line_out2 <- renderPlot({
    
    #Stack plot (堆疊圖)
    ggplot(df2, aes(x=hour, y=rate.sbi, fill=is.rain)) +
      geom_bar(stat="identity") + 
      labs(x="時間", y="有車率") +
      thm() +
      theme(legend.title=element_blank())
  
    
  })
  
  
  output$line_out3 <- renderPlot({
    
    #Dodge plot
    ggplot(df2, aes(x=hour, y=rate.sbi, fill=is.rain)) +
      geom_bar(stat="identity", position="dodge") + 
      labs(x="時間", y="有車率") +
      thm() +
      theme(legend.title=element_blank())
    
    
  })
  
  
  output$line_out4 <- renderPlot({
    
    #Fill plot
    ggplot(df2, aes(x=hour, y=rate.sbi, fill=is.rain)) +
      geom_bar(stat="identity", position="fill") + 
      labs(x="時間", y="相對有車率") +
      thm() +
      theme(legend.title=element_blank())
    
    
  })
  
  
  
  output$line_out5 <- renderPlot({
    
    #Facet panels in a grid
    ggplot(df2, aes(x=hour, y=rate.sbi, fill=is.rain)) +
      geom_bar(stat="identity", position="dodge") + 
      labs(x="時間", y="有車率") +
      thm() +
      theme(legend.title=element_blank()) + 
      facet_grid(is.rain~.)
    
    
  })
  
  output$line_out6 <- renderPlot({
    
    #Facet panels in a grid
    ggplot(df2, aes(x=hour, y=rate.sbi, fill=is.rain)) +
      geom_bar(stat="identity", position="dodge") + 
      labs(x="時間", y="有車率") +
      thm() +
      theme(legend.title=element_blank()) + 
      facet_grid(.~is.rain)
    
    
  }) 
  
  
  
  
  # bus station profile ---------------------------------------------------
  
  output$bus_out1 <- renderPlot({
    
    #Pyramid
    ggplot(df2, aes(x=hour,y=rate.sbi, fill=is.rain)) + 
      geom_bar(data=filter(df2, is.rain=="晴天"), stat="identity") + 
      geom_bar(aes(y=rate.sbi*(-1)), data=filter(df2, is.rain=="雨天"), 
               stat="identity") + 
      scale_y_continuous(breaks=seq(from=-1, to=1, by=0.1), 
                         labels=abs(seq(-1, 1, 0.1))) + 
      labs(x="時間", y="有車率") +
      theme(legend.title=element_blank()) + 
      coord_flip() + thm() 
    
  })
  
  output$bus_out2 <- renderPlot({
    
    #由以下折線圖可知，不管在哪個時段，晴天的有車率都低於雨天，而差異最大的時段是午夜十二點前後 (Why?)，最小的時段是凌晨五點前後 (Why?)。
    ggplot(df2, aes(x=hour, y=rate.sbi, colour=is.rain, fill=is.rain)) +
      geom_line(size=1) + 
      labs(x="時間", y="有車率") +
      thm() +
      theme(legend.title=element_blank())
    
  })
  
  output$bus_out3 <- renderPlot({
    
    #透過統計方法 (loess method)，可以迅速地描繪出平滑的趨勢線 (陰影處為95%信賴區間)。
    #Hint: stat_smooth()
    ggplot(df2, aes(x=hour, y=rate.sbi, colour=is.rain, fill=is.rain)) +
      #geom_line() + 
      labs(x="時間", y="有車率") +
      thm() +
      theme(legend.title=element_blank()) +
      stat_smooth(size=1)
    
  })
  
  output$bus_out4 <- renderPlot({
    
    #另一種呈現方式，又稱雷達圖 (radar chart)，賦予資料面積的概念。
    #Hint: coord_polar()
    
    ggplot(df2, aes(x=hour, y=rate.sbi, colour=is.rain, fill=is.rain)) +
      #geom_line() + 
      labs(x="時間", y="有車率") +
      thm() +
      theme(legend.title=element_blank()) +
      stat_smooth(size=1) + coord_polar()
    
  })
  
  output$bus_out5 <- renderPlot({
    
    #熱點圖 (heatmap) 是用顏色深淺呈現數值大小的視覺化。
    #Hint: geom_tile()
    ggplot(df2, aes(x=hour, y=is.rain, fill=rate.sbi)) + 
      geom_tile() +
      scale_fill_gradient(name="有車率", low="white", high="midnightblue") + 
      labs(x="時間", y="天氣") +
      thm()
    
  })
  
  
  output$bus_out6 <- renderPlot({
    df2 <- mutate(df2, rain=as.numeric(is.rain)-1)
   
    
    ggparcoord(as.data.frame(df2), columns = c(1,4,3), groupColumn = 2) +
      thm() + theme(legend.title=element_blank())
    
  })
  
    output$bus_out7 <- renderPlot({
    
    ggparcoord(data = iris, columns = 1:4, groupColumn = 5,
               title = "Parallel Coordinate Plot for the Iris Data") + thm()
  })
  
  
  
 # transit OD ------------------------------------------------------------
 
 output$od_table1 <- function(){
   paste (
   
   
   
   kable(head(df3), format="html", digits=3, align="c")
   
   )}
  
 output$od_out1 <- renderPlot({
   
   #散佈圖 (scatterplot) 是比較兩數值變數最直覺的視覺化，主要是觀察兩變數間是否存在特殊的趨勢、群聚現象、離群值。以下方式常用來輔助散佈圖的探索：
   
   ggplot(df3, aes(x=rate.used, y=rate.sbi)) + geom_point() + 
     labs(x="使用率", y="有車率") + thm()
   
 })
 
 
 output$od_out2 <- renderPlot({
   
   #散佈圖 (scatterplot) 是比較兩數值變數最直覺的視覺化，主要是觀察兩變數間是否存在特殊的趨勢、群聚現象、離群值。以下方式常用來輔助散佈圖的探索：
   
   ggplot(df3, aes(x=rate.used, y=rate.sbi)) + geom_point() + 
     labs(x="使用率", y="有車率") + thm()+
     geom_vline(xintercept=0.4, lty=2) +
     geom_hline(yintercept=0.4, lty=2) 
   
 })
 
 output$od_out3 <- renderPlot({
   
   #利用顏色分類
   ggplot(df3, aes(x=rate.used, y=rate.sbi)) + 
     labs(x="使用率", y="有車率") + thm() +
     geom_point(aes(colour=is.rushhours), position="jitter")
 })
 
 output$od_out4 <- renderPlot({
   
   ggplot(df3, aes(x=rate.used, y=rate.sbi)) + 
     geom_point(aes(colour=is.rushhours, shape=is.weekday), position="jitter") +
     facet_grid(is.weekday~is.rushhours) + 
     labs(x="使用率", y="有車率") +
     thm() +
     theme(legend.title=element_blank())
   
   
 })
 
 output$od_out5 <- renderPlot({
   
   
   
   ggplot(df4, aes(x=rate.used, y=rate.sbi)) + 
     geom_point(aes(colour=paste(as.character(is.weekday),
                                 as.character(is.rain), sep="-"))) +
     ggtitle("每日7-9點 YouBike使用狀況") +
     labs(x="使用率", y="有車率") +
     facet_grid(is.rain~is.weekday) + 
     thm() +
     theme(legend.title=element_blank())
   
 })
 
 output$od_table2  <- function(){
   paste (
     
   
   
   kable(df5, format="html", digits=3, align="c")
   
   )}
 
 #map
 
 output$map_out1 <-  renderPlot({
   
   df5$is.cityhall <- factor(c(1, rep(0, 9)), levels=1:0)
   map <- get_map(location=c(lon=df5$lng[1], lat=df5$lat[1]) , zoom = 15)
   ggmap(map) + thm() +
     geom_point(data=df5, aes(x=lng, y=lat, colour=is.cityhall), size=5) + 
     geom_text(data=df5, aes(x=lng, y=lat, label=sna, colour=is.cityhall), 
               position="jitter", vjust=-1, hjust=0.5, size=4, family="STHeiti") + 
     theme(legend.position="none") + scale_color_brewer(palette="Set1")
   
 })
 
 
 output$map_out2 <- renderPlot({
   
   #練習用geom_point(size=tot)來改變場站標示的大小。
   df5$is.cityhall <- factor(c(1, rep(0, 9)), levels=1:0)
   df5 <- group_by(tmp) %>% select(sno, tot) %>% 
     right_join(df5, by="sno") %>%
     `[`(c(1, 3, 4, 2, 5, 6, 7, 8)) 
   map <- get_map(location=c(lon=df5$lng[1], lat=df5$lat[1]) , 
                  maptype = "roadmap", zoom = 15)
   
   ggmap(map) + thm() +
     geom_point(data=df5, aes(x=lng, y=lat, colour=is.cityhall, size=tot)) + 
     theme(legend.position="none") + scale_color_brewer(palette="Set1") +
     scale_size(range = c(3,12))
   
 })
 
 
 output$map_out3 <- renderPlot({
   
   #熱點圖進階應用
   
   
   ggplot(df6, aes(x=hour, y=sna, fill=rate.sbi)) + geom_tile() + thm() + 
     theme(legend.position="bottom") + 
     scale_fill_gradient(name="有車率", low="white", high="lawngreen") + 
     labs(x="時間", y="") +
     theme(axis.text = element_text(size = 13, color="darkgreen"))
   
 })
 
 
 output$map_out4 <- renderPlot({
   
   ggplot(df6, aes(x=hour, y=sna, fill=rate.used)) + geom_tile() + thm() + 
     theme(legend.position="bottom") + 
     scale_fill_gradient(name="使用率", low="white", high="Navy") + 
     labs(x="時間", y="") +
     theme(axis.text = element_text(size = 13, color="darkblue"))
   
 })
 
 
  
 
 
 output$map_table1 <- function(){
   dat <- dcast(df6, sna~hour, value.var="rate.sbi")
   rownames(dat) <- dat[,1]
   dat <- dat[,-1]
   paste (
   
   #heatmap 排序
   
   kable(dat[,8:13], format = "html", digits = 3, row.names = TRUE)
   
   )}
 
 
 
 
 output$map_table2 <- function(){
   paste (
   
  
   kable(sna.order, format = "html")
   
   ) }
 
 
 
 output$map_out5 <- renderPlot({
   
  
   ggplot(df7, aes(x=hour, y=sna, fill=rate.sbi)) + geom_tile() + thm() + 
     theme(legend.position="bottom") + 
     scale_fill_gradient(name="有車率", low="white", high="lawngreen") + 
     labs(x="時間", y="") +
     theme(axis.text = element_text(size = 13, color="darkgreen"))
   
 })
 
 
 output$map_out6 <- renderPlot({
   
   #對時間做排序
   hc.hour <- hclust(dist(t(dat)))
   ggdendrogram(hc.hour) + thm() + labs(x="", y="")
 })
   output$map_out7 <- renderPlot({
   hour.order <- data.frame(order=1:24, sna=hc.hour$labels[hc.hour$order])
   # kable(hour.order, format = "html")
   
   df7$hour <- factor(df7$hour, levels=(hour.order[,2]))
   
   ggplot(df7, aes(x=hour, y=sna, fill=rate.sbi)) + geom_tile() + thm() + 
     theme(legend.position="bottom") + 
     scale_fill_gradient(name="有車率", low="white", high="lawngreen") + 
     labs(x="時間", y="") +
     theme(axis.text = element_text(size = 13, color="darkgreen"))
   
   
 })
 
 
 output$map_out8 <- renderPlot({
   
   #試著對 使用率 進行排序
  
   ggplot(df8, aes(x=hour, y=sna, fill=rate.used)) + geom_tile() + thm() + 
     theme(legend.position="bottom") + 
     scale_fill_gradient(name="使用率", low="white", high="Navy") + 
     labs(x="時間", y="") +
     theme(axis.text = element_text(size = 13, color="darkblue"))
   
 })
 
 #kmeans
 output$kmeans_out1 <- renderPlot({
   
   #平行座標圖進階應用
   #平行座標圖常用來展示不同群組在諸多變數間的差異性，當群組分類方式未知時，可以利用機器學習 (machine learning) 中的非監督式學習 (unsupervised learning)，幫資料做分群。分群之後再藉由平行座標圖來呈現資料的脈絡。
   #
   #選擇 平日, 晴天, 7-21時鄰近市府站的資料進行分析
   #以場站大小 (tot)、有車率 (rate.sbi)、使用率 (rate.used) 三個變數做分群
   #使用K-means演算法分3群
   #將分群結果視作新的變數畫平行座標圖
  
   
   ggparcoord(as.data.frame(df9), columns = c(1,2,3,4), groupColumn = 5,
              scale="uniminmax") + 
     geom_line(size=1) + thm() + theme(legend.title=element_blank()) +
     scale_x_discrete(labels=c("場站","總停車格","有車率","使用率")) +
     labs(x="", y="")
   
  
   
 })
 
 
 
 output$kmeans_table1 <- function(){
   paste (
     
   
   kable(df9, format="html", digits=3, align="c")
   
   )}
 
 

 
 
}




