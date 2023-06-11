library(shiny)
library(shinydashboard)
library(plotly)

# Define the user interface
ui <- dashboardPage(
    dashboardHeader(title = "Email Campaign"),
    dashboardSidebar(
        tags$style(type = 'text/css', 
                   "#emails_opened, #click_throughs, #prior_mean, #belief_strength {font-size: 30px;}"),
        numericInput("emails_opened", "Emails Opened:", 5),
        numericInput("click_throughs", "Click-Throughs:", 3),
        numericInput("prior_mean", "Prior Mean:", 0.03),
        sliderInput("belief_strength", "Belief Strength:", min = 0, max = 1, value = 0.5)
    ),
    dashboardBody(
        fluidRow(
            box(plotlyOutput("distPlot"), width = 12)
        ),
        fluidRow(
            box(
                width = 4,
                title = "Prior Mean CTR", solidHeader = TRUE,
                tags$div(style = "text-align:center; font-size: 30px;", textOutput("prior_mean_out"))
            ),
            box(
                width = 4,
                title = "Posterior Mean CTR", solidHeader = TRUE,
                tags$div(style = "text-align:center; font-size: 30px;", textOutput("posterior_mean_out"))
            ),
            box(
                width = 4,
                title = "Chance of uplift", solidHeader = TRUE,
                tags$div(style = "text-align:center; font-size: 30px;", textOutput("prob_increase"))
            )
        )
    )
)

# Define the server logic
server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
        # Set the range of click-through probabilities
        xs <- seq(0, 1, by = 0.01)
        
        # Set the parameters for the prior distribution
        prior_mean <- input$prior_mean
        belief_strength <- input$belief_strength
        alpha_prior <- prior_mean * belief_strength * 100
        beta_prior <- (1 - prior_mean) * belief_strength * 100
        
        # Calculate the prior density
        prior_density <- dbeta(xs, alpha_prior, beta_prior)
        
        # Calculate the likelihood
        likelihood <- dbinom(input$click_throughs, input$emails_opened, xs)
        # likelihood <- likelihood / max(likelihood)
        
        # Calculate the posterior density
        alpha_posterior <- alpha_prior + input$click_throughs
        beta_posterior <- beta_prior + input$emails_opened - input$click_throughs
        posterior_density <- dbeta(xs, alpha_posterior, beta_posterior)
        
        # Create the plot
        p <- plot_ly() %>%
            add_trace(x = xs, y = prior_density, mode = 'lines', name = 'Prior', yaxis = "y") %>%
            add_trace(x = xs, y = likelihood, mode = 'lines', name = 'Likelihood', yaxis = "y2") %>%
            add_trace(x = xs, y = posterior_density, mode = 'lines', name = 'Posterior', yaxis = "y") %>%
            layout(
                title = list(text = "Click-Through Rates", font = list(size = 20, color = "black")),
                xaxis = list(title = "Click-Through Rate", titlefont = list(size = 15)),
                yaxis = list(title = "Probability Density", titlefont = list(size = 15)),
                yaxis2 = list(title = "Likelihood", overlaying = "y", side = "right", titlefont = list(size = 15)),
                margin = list(t = 50) 
            )
        
        # Return the plot
        return(p)
    })
    
    # Output the expected click through rates
    output$prior_mean_out <- renderText({
        paste(round(input$prior_mean * 100, 1), "%")
    })
    
    output$posterior_mean_out <- renderText({
        posterior_mean <- (input$prior_mean * input$belief_strength * 100 + input$click_throughs) / 
            (input$belief_strength * 100 + input$emails_opened)
        paste(round(posterior_mean * 100, 1), "%")
    })
    
    # Output the probability that the click-through rate has increased
    output$prob_increase <- renderText({
        prior_mean <- input$prior_mean
        belief_strength <- input$belief_strength
        alpha_prior <- prior_mean * belief_strength * 100
        beta_prior <- (1 - prior_mean) * belief_strength * 100
        alpha_posterior <- alpha_prior + input$click_throughs
        beta_posterior <- beta_prior + input$emails_opened - input$click_throughs
        
        posterior_mean <- (input$prior_mean * input$belief_strength * 100 + input$click_throughs) / 
            (input$belief_strength * 100 + input$emails_opened)
        prob_increase <- 1 - pbeta(prior_mean, alpha_posterior, beta_posterior)
        paste(round(prob_increase * 100, 1), "%")
    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
