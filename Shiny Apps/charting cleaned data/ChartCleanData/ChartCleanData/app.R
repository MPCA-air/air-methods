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


shinyApp(
  ui = fluidPage(responsive = FALSE,
                 fluidRow(
                   column(3,
                          style = "padding-bottom: 20px;",
                          inputPanel(
                            selectInput("pollutant", label="Choose a pollutant", choices = pollutant, selected="Benzene"),
                            selectInput("site", label="Choose a site", choices = site, selected=270535501))),
                   column(9,
                          plotOutput('detlim', height = "400px")))),
  
  
  server = function(input, output) {
    
    
    
    
    output$detlim <- renderPlot({
      print(input$pollutant)
      print(input$site)
      data_sub = filter(data, pollutant==input$pollutant, aqs_id == input$site)
      data_sub$Censored <- ifelse(data_sub$conc > data_sub$md_limit, FALSE, TRUE)
      mdl <- mean(data_sub$md_limit)
      ggplot(data=data_sub, aes(x= date, y=conc)) +
        geom_point(aes(color=Censored), size =2.4, alpha=0.55) +
        geom_hline(yintercept=mdl) +
        xlab(NULL) +
        ylab("Result (ug/m3)") +
        expand_limits(y=c(0, max(data_sub$conc))) +
        scale_colour_manual(values= c("#197519"[FALSE %in% unique(data_sub$Censored)], "#0000FF"[TRUE %in% unique(data_sub$Censored)]), breaks=c(FALSE, TRUE)) +
        theme(text = element_text(size=15), axis.text.x = element_text(angle = -90, vjust = 0.3,  size=14)) +
        ggtitle(paste0("Time series for ", input$pollutant, " at site ", input$site),
                subtitle = "---- Horizontal Line ----  =  Detection Limit")
    })
    
  })
