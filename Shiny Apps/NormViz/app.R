library(shiny)
library(readr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(RcppRoll)
library(lubridate)
library(DT)
library(rsconnect)

data <- read_csv('https://raw.githubusercontent.com/MPCA-air/air-methods/master/airtoxics_data_2009_2013.csv')
colnames(data) <- c("aqs_id", "poc", "param_code", "date", "conc", "null_code", "md_limit", "pollutant", "year", "cas")
pollutant <- unique(data$pollutant)
site <- unique(data$aqs_id)
year <- unique(data$year)


shinyApp(
  ui = fluidPage(responsive = FALSE,
                 fluidRow(
                   column(3,
                          style = "padding-bottom: 20px;",
                          inputPanel(
                            selectInput("pollutant", label="Choose a pollutant", choices = pollutant, selected="Benzene"),
                            selectInput("year", label="Choose a year", choices = year, selected=2009),
                            selectInput("site", label="Choose a site", choices = site, selected=270535501))),
                   column(9,
                          plotOutput('normviz', height = "400px")))),
  
  
  server = function(input, output) {
    
    
    
    
    output$normviz <- renderPlot({
      print(input$pollutant)
      print(input$site)
      print(input$year)
      data_sub = filter(data, pollutant==input$pollutant, aqs_id == input$site, year == input$year)
      par(mfrow=c(2,2))
      qqnorm(data_sub$conc)
      hist(data_sub$conc)
    })
    
  })