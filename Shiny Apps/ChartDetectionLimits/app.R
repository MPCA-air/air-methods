library(shiny)
library(rsconnect)
library(readr)
library(ggplot2)
library(dplyr)
data <- read_csv('https://raw.githubusercontent.com/MPCA-air/air-methods/master/airtoxics_data_2009_2013.csv')

colnames(data) <- c("aqs_id", "poc", "param_code", "date", "conc", "null_code", "md_limit", "pollutant", "year", "cas")
pollutant <- unique(data$pollutant)
year <- unique(data$year)

shinyApp(
  ui = fluidPage(responsive = FALSE,
                 fluidRow(
                   column(3,
                          style = "padding-bottom: 20px;",
                          inputPanel(
                            selectInput("pollutant", label="Choose a pollutant", choices = pollutant, selected="Benzene"),
                            selectInput("year", label="Choose a year", choices = year, selected=2009))),
                   column(9,
                          plotOutput('detlim', height = "400px")))),
  
  
  server = function(input, output) {
    
    
    
    output$detlim <- renderPlot({
      print(input$year)
      print(input$pollutant)
      data_sub = filter(data, year==input$year, pollutant==input$pollutant)
      mdl <- mean(data_sub$md_limit)
      data_sub$Censored <- ifelse(data_sub$conc > data_sub$md_limit, FALSE, TRUE)
      ggplot(data=data_sub, aes(x= factor(aqs_id), y=conc)) +
        scale_colour_manual(values= c("#197519"[FALSE %in% unique(data_sub$Censored)], "#0000FF"[TRUE %in% unique(data_sub$Censored)]), breaks=c(FALSE, TRUE)) +
        geom_boxplot(outlier.colour=NA) +
        geom_jitter(aes(color=Censored), size =2.4, alpha=0.55) +
        geom_hline(yintercept=mdl) +
        xlab(NULL) +
        ylab("Result (ug/m3)") +
        expand_limits(y=c(0, max(data_sub$conc))) +
        theme(text = element_text(size=15), axis.text.x = element_text(angle = -90, vjust = 0.3,  size=14)) +
        ggtitle(paste0("Site Comparison Boxplot with Censored and Non-Censored Values for ", input$pollutant, ", from ", input$year),
                subtitle = "---- Horizontal Line ----  =  Detection Limit")
    })
    
  })