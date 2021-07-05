library(dplyr)
library(data.table)
library(ggplot2)

setwd("D:\\data_science\\Projects\\Temp_blanket\\data")

dfWeather <- readRDS("2020_TEMP.RDS")

dfWeather <- dfWeather %>% filter(STATE == "MO")
dfWeather <- dfWeather %>% filter(NAME == "ST JOSEPH ROSECRANS AP")

dfColorMap <- read.csv("color_map.txt")

setDT(dfWeather)
setDT(dfColorMap)

dfGraphData <- dfWeather[dfColorMap, on = .(temp_in_f >= lower_bound, temp_in_f <= upper_bound), nomatch = 0]
dfGraphData$OBS_DT <- as.character(dfGraphData$OBS_DT)
setDF(dfGraphData)
dfGraphData$dummy <- 1

dfColors <- read.csv("colors.txt")
dfColors$html_code <- as.character(dfColors$html_code)

dfGraphData$html_code <- as.character(dfGraphData$html_code)
dfGraphData$color_name <- as.character(dfGraphData$color_name)

dfGraphData$color_name <- factor(dfGraphData$color_name, levels = dfColors$color_name)


ggplot(dfGraphData, aes(x = OBS_DT, y = dummy, fill = color_name)) +
  geom_col(width=1) +
  scale_fill_manual(values = dfColors$html_code)




