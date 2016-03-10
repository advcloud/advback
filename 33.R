library(plyr)
library(rCharts)
library(scales)
library(httr)
library(RCurl)
library(XML)
library(jsonlite)

# get database
#resource ID}?filters={filters}&q={q}&sort={sort}&limit={limit}&offset={offset}

url1<- 'http://data.tycg.gov.tw/TYCG_OPD/api/v1/rest/datastore/3a2c7adb-c912-4eac-a9de-b08149a23fc2?format=json&limit=700'
url2 <- 'http://data.tycg.gov.tw/TYCG_OPD/api/v1/rest/datastore/49a0a8da-098e-4d29-9f46-6924d28d163c?format=json&limit=700'
url3 <- 'http://data.tycg.gov.tw/TYCG_OPD/api/v1/rest/datastore/39f01c94-c487-402d-b763-20fe01ad0d1b?format=json&limit=700'
y <- fromJSON(url1, flatten = TRUE)
y1 <- as.data.frame(y$result$records)
y <- fromJSON(url2, flatten = TRUE)
y2 <- as.data.frame(y$result$records)
y9 <- rbind(y1, y2)
names(y9) <- c('type','lon','breau','month','station','year','lat','_id')

y <- fromJSON(url3)
y3 <- as.data.frame(y$result$records)
y4 <- y3 [,c('type','lon','breau','month','station','year','lat','_id')]
#y5 <- as.matrix(y4)

y7 <- rbind(y9, y4)
y7$lon <- as.numeric(as.character(y7$lon))
y7$lat <- as.numeric(as.character(y7$lat))
#y7$type <-  as.character(y7$type)
data <- y7