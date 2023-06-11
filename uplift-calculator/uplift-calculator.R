library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(coda)

source("conversion-rate-functions.R")


ui <- fluidPage(
    theme = shinytheme("darkly"),
    titlePanel("Conversion Rate Uplift Estimator"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("impressions1",
                        "Impressions (Test 1):",
                        min = 0,
                        max = 5000,
                        value = 1000, 
                        step = 10),
            sliderInput("conversions1",
                        "Conversions (Test 1):",
                        min = 0,
                        max = 50,
                        value = 19),
            sliderInput("impressions2",
                        "Impressions (Test 2):",
                        min = 0,
                        max = 5000,
                        value = 800, 
                        step = 10),
            sliderInput("conversions2",
                        "Conversions (Test 2):",
                        min = 0,
                        max = 50,
                        value = 20)
        ),
        
        mainPanel(
            plotOutput("conversionPlot")
        )
    )
)

server <- function(input, output) {
    output$conversionPlot <- renderPlot({
        plot_conversion_rate_uplift(input$impressions1, 
                                    input$conversions1, 
                                    input$impressions2, 
                                    input$conversions2)
    })
}

shinyApp(ui = ui, server = server)
