#make rds
library(data.table)
library(dplyr)

setwd("D:\\data_science\\Projects\\Temp_blanket\\data")

#read in weather file and filter to daily high and US weather stations
dtTemp <- fread("2020.csv", header = FALSE, stringsAsFactors = FALSE, col.names = c("ID","OBS_DT","ELEMENT","VALUE","M_FLAG","Q_FLAG","S_FLAG","OBS_TIME"))
dtTemp <- dtTemp %>% filter(ELEMENT == "TMAX")
dtTemp <- dtTemp %>% filter(ID %like% "US")

dtTemp$temp_in_f <- round(((dtTemp$VALUE/10) * (9/5)) + 32,0)

Schema <- read.table(text = "ID            1-11   Character
LATITUDE     13-20   Real
LONGITUDE    22-30   Real
ELEVATION    32-37   Real
STATE        39-40   Character
NAME         42-71   Character
GSNFLAG      73-75   Character
HCNFLAG      77-79   Character
WMOID        81-85   Character", header = FALSE, stringsAsFactors = FALSE)

Widths <- as.numeric(sapply(strsplit(as.character(Schema$V2), "-"), `[`, 2))
Widths <- c(Widths[1], diff(Widths))

dfStations <- read.fwf("ghcnd-stations.txt", widths = Widths, col.names = Schema$V1, strip.white = TRUE, comment.char = "")

dfStationsUS <- dfStations %>% filter(ID %like% "US")

#Join the weather data to the station dim
dfJoin <- inner_join(dtTemp, dfStationsUS, by = c("ID" = "ID"))

dfStationsWithWeather <- dfJoin %>% select(ID) %>% unique()

dfJoin2 <- inner_join(dfStationsUS, dfStationsWithWeather, by = c("ID" = "ID"))

saveRDS(dfJoin2, "US_STATIONS.RDS")
saveRDS(dfJoin,"2020_TEMP.RDS")
