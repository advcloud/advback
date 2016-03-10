library(dplyr)

# get database
WP.basin = read.csv(" WPbasin.csv ",  stringsAsFactors = FALSE)
WP.basin <- WP.basin[-1,]
WP.basin.df <- tbl_df(WP.basin)
WP.df <- mutate(WP.basin.df, 
                Season = as.numeric(Season),
                Latitude = as.numeric(gsub("^ ", "", Latitude)),
                Longitude = as.numeric(gsub("^ ", "", Longitude)),
                Wind.WMO. = as.numeric(gsub("^ ", "", Wind.WMO.)) * 0.5144,
                ISO_time = as.POSIXct(ISO_time),
                Month = factor(substr(ISO_time, 6,7), labels = c(month.name))
)
substorms <- WP.df %.% 
  filter(Season %in% 1999:2010 & !(Name == "NOT NAMED")) %.%
  mutate(ID = as.factor(paste(Name, Season, sep = ".")))
