library(plyr)
library(rCharts)
library(scales)
library(httr)
library(RCurl)
library(XML)
library(jsonlite)

# get database
url_gen = "http://opendata.epa.gov.tw/ws/Data/AQX/?format=xml"
url_gen1 = "http://opendata.epa.gov.tw/ws/Data/AQXSite/?format=xml"
data = xmlToDataFrame(url_gen)
data2 <- read.csv(file='AQXSite.csv')
data3 <- merge(data2,data,by='SiteName')
WaterData<-fromJSON("http://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=190796c8-7c56-42e0-8068-39242b8ec927")

WaterDataFrame<-WaterData$result$results
WaterDataFrame$longitude<-as.numeric(WaterDataFrame$longitude)
WaterDataFrame$latitude<-as.numeric(WaterDataFrame$latitude)
WaterDataFrame$qua_cntu<-as.numeric(WaterDataFrame$qua_cntu)

