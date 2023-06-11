source("ecdf-functions.R")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(plotly)
library(tidyverse)

ui <- dashboardPage(
    dashboardHeader(title = "Uplift A/B Test"),
    dashboardSidebar(
        numericInput("prior_alpha", "Prior Alpha:", 30),
        numericInput("prior_beta", "Prior Beta:", 70),
        numericInput("a_successes", "A - Successes:", 36),
        numericInput("a_total", "A - Total:", 150),
        numericInput("b_successes", "B - Successes:", 50),
        numericInput("b_total", "B - Total:", 150),
        sliderInput("hdi_low", "HDI Low:", min = 0, max = 1, value = 0.25),
        sliderInput("hdi_high", "HDI High:", min = 0, max = 1, value = 0.75),
        actionButton("run", "Run Analysis", 
                     class = "btn-primary", 
                     style = "color: white; background-color: red; font-weight: bold")
    ),
    dashboardBody(
        useShinyjs(),
        tags$head(tags$style(HTML(".skin-blue .main-header .logo {background-color: #222d32;}
                                  .skin-blue .main-header .navbar {background-color: #222d32;}
                                  .skin-blue .main-sidebar {background-color: #222d32;}
                                  .main-header .logo {
                                    font-size: 30px;
                                  }
                                  .form-control {
                                    font-size: 18px;
                                  }
                                  .irs-grid-text {
                                    font-size: 10px;
                                  }
                               "))),
        tags$style(HTML("
            body {
              font-size: 20px;
            }
            ")),
        plotlyOutput("uplift_plot", height = "800px")
    )
)

server <- function(input, output) {
    observeEvent(input$run, {
        df <- sample_uplift(
            prior_alpha = input$prior_alpha,
            prior_beta = input$prior_beta,
            a_successes = input$a_successes,
            a_total = input$a_total,
            b_successes = input$b_successes,
            b_total = input$b_total,
            n_trials = 50000
        )
        
        p <- plot_uplift_ecdf(df = df, 
                              hdi_low = input$hdi_low, 
                              hdi_high = input$hdi_high)
        
        output$uplift_plot <- renderPlotly({p})
    })
}

shinyApp(ui = ui, server = server)
