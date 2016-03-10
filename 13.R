library(plyr)
library(rCharts)
library(scales)
library(httr)
library(RCurl)
library(XML)
library(jsonlite)

# get database
url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
airport <- read.csv(url, header = F, stringsAsFactor = F)
airports1 <- airport 
colnames(airports1) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")

url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"
route <- read.csv(url, header = F, stringsAsFactors = F)
routes1 <- route
colnames(routes1) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")

airport <- airport[airport$V5!='', c('V3', 'V4', 'V5','V7','V8')]
names(airport) <- c("City", "Country", "IATA", "lantitude", 
                    "longitude")

route <- route[c('V1', 'V3', 'V5')]
names(route) <- c("Airline", "Departure", "Arrival")
airport.krjp <- subset(airport, Country %in% c("South Korea", "Japan"))
route.krjp <- subset(route, (Departure %in% airport.krjp$IATA &
                               Arrival %in% airport.krjp$IATA))

findposition <- function(IATA) {
  find <- airport.krjp$IATA==IATA
  x <- airport.krjp[find,'longitude']
  y <- airport.krjp[find,'lantitude']
  return(data.frame(x,y))
}

from <- lapply(route.krjp$Departure,findposition)
from <- do.call('rbind',from)
from$group <- c(1:nrow(from))
names(from) <- c("longitude",'lantitude','group')

to <- lapply(route.krjp$Arrival,findposition)
to <- do.call('rbind',to)
to$group <- c(1:nrow(to))
names(to) <-c('longitude','lantitude','group')
data.line <- rbind(from,to)

Arriving.freq <- data.frame(table(route.krjp$Arrival))
names(Arriving.freq) <- c('IATA', 'Freq')
airport.krjp <- merge(airport.krjp, Arriving.freq, by = c("IATA"), all=T)
airport.krjp <- airport.krjp[order(airport.krjp$Freq, decreasing=T),]
### Top 10

departures <- ddply(routes1, .(sourceAirportID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes1, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"
airportD <- merge(airports1, departures, by.x = "ID", by.y = "sourceAirportID")
airportA <- merge(airports1, arrivals, by.x = "ID", by.y = "destinationAirportID")
airportD$type <- "departures"
airportA$type <- "arrivals"
airportDA <- rbind(airportD, airportA)