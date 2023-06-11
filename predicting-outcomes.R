library(tidyverse)
library(ggplot2)
library(gt)
library(tidyr)

# Function to select the prior function ----
select_prior <- function(prior_type = "uniform", alpha = 2, beta = 2) {
    if (prior_type == "uniform") {
        prior_function <- function(pi_vals) {
            return(rep(1, length(pi_vals)))
        }
    } else if (prior_type == "normal") {
        prior_function <- function(pi_vals) {
            mu <- 0.5
            sigma <- 0.1
            return(dnorm(pi_vals, mean = mu, sd = sigma))
        }
    } else if (prior_type == "beta") {
        prior_function <- function(pi_vals) {
            return(dbeta(pi_vals, shape1 = alpha, shape2 = beta))
        }
    } else {
        stop("Invalid prior type")
    }
    
    # Preview prior distribution
    pi_vals <- seq(0, 1, by = 0.01)
    prior <- prior_function(pi_vals)
    data <- tibble(pi = pi_vals, prior = prior)
    g <- ggplot(data, aes(x = pi, y = prior)) +
        geom_line() +
        labs(title = paste("Prior chances of team win probability"),
             x = "Win Probability",
             y = "Prior Probability") +
        theme_minimal()
    
    print(g)
    
    return(prior_function)
}

# Example usage ----
uniform_prior <- select_prior("uniform")
normal_prior <- select_prior("normal")
beta_prior <- select_prior("beta", alpha = 2, beta = 5)


# Function to plot posterior probability given games won and a prior function ----
posterior_probability_plot <- function(games_played, games_won, prior_function) {
    n <- games_played
    k <- games_won
    
    pi_vals <- seq(0, 1, by = 0.01)
    likelihoods <- dbinom(k, size = n, prob = pi_vals)
    prior <- prior_function(pi_vals)
    
    # Calculate the unnormalized posterior by multiplying the likelihood and the prior
    unnormalized_posterior <- likelihoods * prior
    
    # Normalize the posterior
    posterior <- unnormalized_posterior / sum(unnormalized_posterior)
    
    data <- tibble(pi = pi_vals, posterior = posterior)
    
    ggplot(data, aes(x = pi, y = posterior)) +
        geom_line() +
        labs(title = paste("Posterior probability of team win probability given", 
                           k, "out of", n, "games won"),
             x = "Win Probability",
             y = "Posterior Probability") +
        theme_minimal()
}

# Function to create posterior probability table with a custom prior function
posterior_probability_table <- function(games_played, prior_function) {
    n <- games_played
    pi_vals <- seq(0.1, 0.9, 0.1)
    games_won <- 0:n
    
    likelihoods <- sapply(games_won, function(k) dbinom(k, size  = n, prob = pi_vals))
    likelihoods <- t(likelihoods)
    
    prior <- matrix(prior_function(pi_vals), 
                    nrow = length(games_won), 
                    ncol = length(pi_vals), 
                    byrow = TRUE)
    
    unnormalized_posterior <- likelihoods * prior
    posterior <- unnormalized_posterior / rowSums(unnormalized_posterior)
    
    row_normalized_posterior <- apply(posterior, 1, function(x) x / max(x))
    
    data <- expand_grid(Games_Won = games_won, Pi = pi_vals)
    data$Posterior <- c(t(posterior))
    data$Row_Normalized_Posterior <- c(row_normalized_posterior)
    
    ggplot(data, aes(x = Pi, y = rev(Games_Won), fill = Row_Normalized_Posterior)) +
        geom_tile() +
        geom_text(aes(label = sprintf("%.3f", Posterior)), 
                  size = 3, color = "black") +
        scale_fill_gradient2(low = "lightblue", mid = "white", 
                             high = "red", midpoint = 0.4) +
        scale_y_continuous(breaks = games_won, labels = rev(games_won)) +
        scale_x_continuous(breaks = pi_vals, 
                           labels = paste0("Pi = ", format(pi_vals, nsmall = 1))) +
        theme_minimal() +
        labs(title = "Posteriors for Team Win Probability Given Past Win/Loss Record",
             x = "Win Probability",
             y = "Games Won") +
        guides(fill = FALSE) 
}

# Example usage
posterior_probability_table(8, prior_function = beta_prior)




posterior_probability_table(games_played = 5, prior_function = function(x) {dbeta(x, 2, 2)})


# Using beta prior
posterior_probability_plot(games_played = 7, 
                           games_won = 2, 
                           prior_function = beta_prior)

posterior_probability_table(8, prior_function = beta_prior)

# Using uniform prior
posterior_probability_plot(games_played = 7, 
                           games_won = 0, 
                           prior_function = beta_prior)

posterior_probability_table(7, prior_function = uniform_prior)

# Using normal prior
posterior_probability_plot(games_played = 7, 
                           games_won = 0, 
                           prior_function = normal_prior)

posterior_probability_table(7, prior_function = normal_prior)


