#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("viridis")
#install.packages("ggthemes")

library(dplyr) #資料處理package
library(ggplot2) #視覺化package
library(viridis) #主題package
library(ggthemes) #配色package

type <- read.csv( "type.csv",fileEncoding="utf-8")
vtype <- read.csv( "vtype.csv",fileEncoding="utf-8")
method <- read.csv( "method.csv",fileEncoding="utf-8")

type_month <- read.csv( "type_month.csv",fileEncoding="utf-8")
vtype_month <- read.csv( "vtype_month.csv",fileEncoding="utf-8")
method_month <- read.csv( "method_month.csv",fileEncoding="utf-8")


top5type <- type[1:5,1]#取出數量前五的種類

top5 <- data.frame()#產生空的框架
for(i in 1:5){
  X <- type_month %>%
    filter(type == as.character(top5type[i]))
  top5 <- rbind(top5,X)
}#重新取出數量前五種類的資料
