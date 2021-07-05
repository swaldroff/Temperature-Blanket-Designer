#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(dplyr)
library(data.table)
library(ggplot2)
library(colourpicker)

getStates <- function(){
  dfStations <- readRDS("..\\data\\US_STATIONS.RDS")
  dfStates <- dfStations %>% select(STATE)
  dfStates <- as.character(unique(dfStates$STATE))
  dfStates <- sort(dfStates)
  return (dfStates)
}

getStations <- function(strState){
  dfStations <- readRDS("..\\data\\US_STATIONS.RDS")
  dfStations <- dfStations %>% filter(STATE == strState)
  dfStations <- dfStations %>% select(NAME)
  dfStations <- sort(as.character(dfStations$NAME))
  return (dfStations)
}

server <- function(input, output) {
  insertUI("head", "beforeEnd", useShinyjs())
  
  dfFilteredWeather <- NULL
  
  
  dfStations <- readRDS("..\\data\\US_STATIONS.RDS")
  dfStates <- dfStations %>% select(STATE)
  dfStates <- as.character(unique(dfStates$STATE))
  dfStates <- sort(dfStates)
    
  output$state <- renderUI({ selectInput("varState", label = "State:", choices = dfStates) })
  selectedState <- reactive({ data <- input$varState })
  
  output$station <- renderUI({
    filterState <- ""
    if (is.null(selectedState())) filterState <- "AK" else filterState <- selectedState()
    selectInput("varStation", label = "Station:", choices = getStations(filterState)) })
  
  selectedStation <- reactive({ data <- input$varStation })
  
  observeEvent(input$btnLoadWeather,
               {
                 dfFilteredWeather <- dfWeather %>% filter(NAME == selectedStation())
                 d <- data.frame(unclass(summary(dfFilteredWeather$temp_in_f)), check.names = FALSE, stringsAsFactors = FALSE)
                 colnames(d) <- c("Temperature")
                 shinyjs::show("lblSummary")
                 output$weatherSummary <- renderTable({d}, rownames = TRUE)
                 
                 
                 output$histTemp <- renderPlot({
                   ggplot(dfFilteredWeather, aes(x=temp_in_f))+
                     geom_histogram(color="darkblue", fill="lightblue")+
                     scale_x_continuous(breaks=seq(-20, 120, by=5))+
                     ggtitle("Histogram of Temperature (in degrees F.)") + xlab("Temperature") + ylab("Count") 
                   })
                 
              
                   shinyjs::showElement(id = "toggleSummary", anim = TRUE)
                   shinyjs::onclick("toggleAdvanced",shinyjs::toggle(id = "toggleSummary", anim = TRUE))
                 
                  
               }
               )
  
  
  
  
  output$cpicker <- renderUI({colourpicker::colourInput("col", "Lookup Colors:", "red")})
  
  observeEvent(input$btnGenerateGraph,
               {
                
                 dfFilteredWeather <- dfWeather %>% filter(NAME == selectedStation())
                 
                 colLowerBounds <- c(input$lb1,input$lb2,input$lb3,input$lb4,input$lb5,input$lb6,input$lb7,input$lb8,input$lb9,input$lb10,input$lb11,input$lb12)
                 colUpperBounds <- c(input$ub1,input$ub2,input$ub3,input$ub4,input$ub5,input$ub6,input$ub7,input$ub8,input$ub9,input$ub10,input$ub11,input$ub12)
                 colHTMLCodes <- c(input$code1,input$code2,input$code3,input$code4,input$code5,input$code6,input$code7,input$code8,input$code9,input$code10,input$code11,input$code12)
                 colColorNames <- c(input$color1,input$color2,input$color3,input$color4,input$color5,input$color6,input$color7,input$color8,input$color9,input$color10,input$color11,input$color12)
                 
                 dfColorMap <- data.frame(colLowerBounds,colUpperBounds,colHTMLCodes,colColorNames, stringsAsFactors = FALSE)
                 #dfColors <- dfColorMap %>% select(colHTMLCodes, colColorNames) %>% unique()
                 
                 dfColorMap$colLowerBounds <- as.double(dfColorMap$colLowerBounds)
                 dfColorMap$colUpperBounds <- as.double(dfColorMap$colUpperBounds)
                 
                 setDT(dfFilteredWeather)
                 setDT(dfColorMap)
                 
                 dfGraphData <- dfFilteredWeather[dfColorMap, on = .(temp_in_f >= colLowerBounds, temp_in_f <= colUpperBounds), nomatch = 0]
                 setDF(dfGraphData)
                 dfGraphData$dummy <- 1
                 
                 dfColors <- dfGraphData %>% select(colHTMLCodes, colColorNames) %>% unique()
                 
                 dfGraphData$OBS_DT <- as.character(dfGraphData$OBS_DT)
                 dfColors$colHTMLCodes <- as.character(dfColors$colHTMLCodes)
                 dfGraphData$colHTMLCodes <- as.character(dfGraphData$colHTMLCodes)
                 
                 dfGraphData$colColorNames <- as.character(dfGraphData$colColorNames)
                 dfGraphData$colColorNames <- factor(dfGraphData$colColorNames, levels = dfColors$colColorNames)
                 
                 
                 # ggplot(dfGraphData, aes(x = OBS_DT, y = dummy, fill = colColorNames)) +
                 #   geom_col(width=1) +
                 #   scale_fill_manual(values = dfColors$colHTMLCodes)
                 
                 output$tempPlot <- renderPlot({
                   ggplot(dfGraphData, aes(x = OBS_DT, y = dummy, fill = colColorNames)) +
                     geom_col(width=1) +
                     scale_fill_manual(values = dfColors$colHTMLCodes, name = "Color") +
                     theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
                           axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
                     xlab("") + ylab("") 
                   })
                 
               }
               )
  
  
  output$lowerBound1 <- renderUI({ textInput("lb1", NULL, "0", "100%") })
  output$upperBound1 <- renderUI({ textInput("ub1", NULL, "9", "100%") })
  output$htmlCode1 <- renderUI({textInput("code1",NULL,"#AAABAA", "100%")})
  output$colorName1 <- renderUI({textInput("color1",NULL,"whisker", "100%")})
  
  output$lowerBound2 <- renderUI({ textInput("lb2", NULL, "10", "100%") })
  output$upperBound2 <- renderUI({ textInput("ub2", NULL, "19", "100%") })
  output$htmlCode2 <- renderUI({textInput("code2",NULL,"#AAABAA", "100%")})
  output$colorName2 <- renderUI({textInput("color2",NULL,"whisker", "100%")})
  
  output$lowerBound3 <- renderUI({ textInput("lb3", NULL, "20", "100%") })
  output$upperBound3 <- renderUI({ textInput("ub3", NULL, "29", "100%") })
  output$htmlCode3 <- renderUI({textInput("code3",NULL,"#077382", "100%")})
  output$colorName3 <- renderUI({textInput("color3",NULL,"swell", "100%")})
  
  output$lowerBound4 <- renderUI({ textInput("lb4", NULL, "30", "100%") })
  output$upperBound4 <- renderUI({ textInput("ub4", NULL, "39", "100%") })
  output$htmlCode4 <- renderUI({textInput("code4",NULL,"#59787B", "100%")})
  output$colorName4 <- renderUI({textInput("color4",NULL,"cadet", "100%")})
  
  output$lowerBound5 <- renderUI({ textInput("lb5", NULL, "40", "100%") })
  output$upperBound5 <- renderUI({ textInput("ub5", NULL, "49", "100%") })
  output$htmlCode5 <- renderUI({textInput("code5",NULL,"#DF9C2A", "100%")})
  output$colorName5 <- renderUI({textInput("color5",NULL,"turmeric", "100%")})
  
  output$lowerBound6 <- renderUI({ textInput("lb6", NULL, "50", "100%") })
  output$upperBound6 <- renderUI({ textInput("ub6", NULL, "59", "100%") })
  output$htmlCode6 <- renderUI({textInput("code6",NULL,"#B68589", "100%")})
  output$colorName6 <- renderUI({textInput("color6",NULL,"ice lily", "100%")})
  
  output$lowerBound7 <- renderUI({ textInput("lb7", NULL, "60", "100%") })
  output$upperBound7 <- renderUI({ textInput("ub7", NULL, "69", "100%") })
  output$htmlCode7 <- renderUI({textInput("code7",NULL,"#EBA7A6", "100%")})
  output$colorName7 <- renderUI({textInput("color7",NULL,"tea rose", "100%")})
  
  output$lowerBound8 <- renderUI({ textInput("lb8", NULL, "70", "100%") })
  output$upperBound8 <- renderUI({ textInput("ub8", NULL, "79", "100%") })
  output$htmlCode8 <- renderUI({textInput("code8",NULL,"#996F83", "100%")})
  output$colorName8 <- renderUI({textInput("color8",NULL,"comfrey", "100%")})
  
  output$lowerBound9 <- renderUI({ textInput("lb9", NULL, "80", "100%") })
  output$upperBound9 <- renderUI({ textInput("ub9", NULL, "89", "100%") })
  output$htmlCode9 <- renderUI({textInput("code9",NULL,"#54336B", "100%")})
  output$colorName9 <- renderUI({textInput("color9",NULL,"eggplant", "100%")})
  
  output$lowerBound10 <- renderUI({ textInput("lb10", NULL, "90", "100%") })
  output$upperBound10 <- renderUI({ textInput("ub10", NULL, "99", "100%") })
  output$htmlCode10 <- renderUI({textInput("code10",NULL,"#972767", "100%")})
  output$colorName10 <- renderUI({textInput("color10",NULL,"fairy tail", "100%")})
  
  output$lowerBound11 <- renderUI({ textInput("lb11", NULL, "100", "100%") })
  output$upperBound11 <- renderUI({ textInput("ub11", NULL, "109", "100%") })
  output$htmlCode11 <- renderUI({textInput("code11",NULL,"#972767", "100%")})
  output$colorName11 <- renderUI({textInput("color11",NULL,"fairy tail", "100%")})
  
  output$lowerBound12 <- renderUI({ textInput("lb12", NULL, "110", "100%") })
  output$upperBound12 <- renderUI({ textInput("ub12", NULL, "119", "100%") })
  output$htmlCode12 <- renderUI({textInput("code12",NULL,"#972767", "100%")})
  output$colorName12 <- renderUI({textInput("color12",NULL,"fairy tail", "100%")})
  dfWeather <- readRDS("..\\data\\2020_TEMP.RDS")
  
  }


ui <- htmlTemplate("www/index.html", 
                   
                   ddState = uiOutput("state"),
                   ddStation = uiOutput("station"),
                   
                   btnLoadWeather = actionButton("btnLoadWeather", "View Weather Summary"),
                   #lblSummary = shinyjs::hidden(p(id = "lblSummary", "Summary of temperature data:")),
                   lnkToggleSummary = actionLink("toggleAdvanced", "Hide Summary"),
                   weatherSummary = uiOutput("weatherSummary"),
                   histTemp = uiOutput("histTemp"),
                   
                   txtLb1 = uiOutput("lowerBound1"),
                   txtUb1 = uiOutput("upperBound1"),
                   txtCode1 = uiOutput("htmlCode1"),
                   txtName1 = uiOutput("colorName1"),
                   
                   txtLb2 = uiOutput("lowerBound2"),
                   txtUb2 = uiOutput("upperBound2"),
                   txtCode2 = uiOutput("htmlCode2"),
                   txtName2 = uiOutput("colorName2"),
                   
                   txtLb3 = uiOutput("lowerBound3"),
                   txtUb3 = uiOutput("upperBound3"),
                   txtCode3 = uiOutput("htmlCode3"),
                   txtName3 = uiOutput("colorName3"),
                   
                   txtLb4 = uiOutput("lowerBound4"),
                   txtUb4 = uiOutput("upperBound4"),
                   txtCode4 = uiOutput("htmlCode4"),
                   txtName4 = uiOutput("colorName4"),
                   
                   txtLb5 = uiOutput("lowerBound5"),
                   txtUb5 = uiOutput("upperBound5"),
                   txtCode5 = uiOutput("htmlCode5"),
                   txtName5 = uiOutput("colorName5"),
                   
                   txtLb6 = uiOutput("lowerBound6"),
                   txtUb6 = uiOutput("upperBound6"),
                   txtCode6 = uiOutput("htmlCode6"),
                   txtName6 = uiOutput("colorName6"),
                   
                   txtLb7 = uiOutput("lowerBound7"),
                   txtUb7 = uiOutput("upperBound7"),
                   txtCode7 = uiOutput("htmlCode7"),
                   txtName7 = uiOutput("colorName7"),
                   
                   txtLb8 = uiOutput("lowerBound8"),
                   txtUb8 = uiOutput("upperBound8"),
                   txtCode8 = uiOutput("htmlCode8"),
                   txtName8 = uiOutput("colorName8"),
                   
                   txtLb9 = uiOutput("lowerBound9"),
                   txtUb9 = uiOutput("upperBound9"),
                   txtCode9 = uiOutput("htmlCode9"),
                   txtName9 = uiOutput("colorName9"),
                   
                   txtLb10 = uiOutput("lowerBound10"),
                   txtUb10 = uiOutput("upperBound10"),
                   txtCode10 = uiOutput("htmlCode10"),
                   txtName10 = uiOutput("colorName10"),
                   
                   txtLb11 = uiOutput("lowerBound11"),
                   txtUb11 = uiOutput("upperBound11"),
                   txtCode11 = uiOutput("htmlCode11"),
                   txtName11 = uiOutput("colorName11"),
                   
                   txtLb12 = uiOutput("lowerBound12"),
                   txtUb12 = uiOutput("upperBound12"),
                   txtCode12 = uiOutput("htmlCode12"),
                   txtName12 = uiOutput("colorName12"),
                   
                   btnGenerateGraph = actionButton("btnGenerateGraph", "Visualize Blanket"),
                   
                   tempPlot = uiOutput("tempPlot"),
                   
                   cpicker = uiOutput("cpicker")
                   )


# Run the application 

shinyApp(ui = ui, server)

