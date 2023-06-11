library(ggplot2)
library(ggthemes)
library(ggpubr)

plot_beta <- function(impressions, conversions) {
    # Create a Beta distribution with parameters alpha = conversions + 1 and beta = impressions - conversions + 1
    x <- seq(0, 1, length.out = 1000)
    y <- dbeta(x, conversions + 1, impressions - conversions + 1)
    
    # Calculate the 95% credible interval
    credible_interval <- qbeta(c(0.025, 0.975), conversions + 1, impressions - conversions + 1)
    
    # Create the plot
    plot <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
        geom_line() +
        theme_minimal() +
        ggtitle("Posterior distribution of conversion rate") +
        xlab("Conversion rate") +
        ylab("Density") +
        geom_vline(xintercept = credible_interval, linetype = "dashed", color = "red") +
        annotate("text", x = mean(credible_interval), y = max(y), 
                 label = paste0("95% CI: [", round(credible_interval[1], 3), ", ", round(credible_interval[2], 3), "]"),
                 hjust = 0, color = "red") +
        scale_x_log10()
    
    # Return the plot
    return(plot)
}

plot_beta(impressions = 1000, conversions = 19)

plot_beta <- function(impressions1, conversions1, impressions2, conversions2) {
    # Calculate the 1% and 99% quantiles for both Beta distributions
    min_x <- pmin(qbeta(c(0.005), conversions1 + 1, impressions1 - conversions1 + 1),
                  qbeta(c(0.005), conversions2 + 1, impressions2 - conversions2 + 1))
    max_x <- pmax(qbeta(c(0.995), conversions1 + 1, impressions1 - conversions1 + 1),
                  qbeta(c(0.995), conversions2 + 1, impressions2 - conversions2 + 1))
    
    # Create two Beta distributions
    x <- seq(min_x, max_x, length.out = 1000)
    y1 <- dbeta(x, conversions1 + 1, impressions1 - conversions1 + 1)
    y2 <- dbeta(x, conversions2 + 1, impressions2 - conversions2 + 1)
    
    # Calculate the 95% credible intervals
    credible_interval1 <- qbeta(c(0.025, 0.975), conversions1 + 1, impressions1 - conversions1 + 1)
    credible_interval2 <- qbeta(c(0.025, 0.975), conversions2 + 1, impressions2 - conversions2 + 1)
    
    # Find the x values that give the maximum density
    max_x1 <- x[which.max(y1)]
    max_x2 <- x[which.max(y2)]
    
    # Create the data frames
    df1 <- data.frame(x = x, y = y1, Group = "Test 1")
    df2 <- data.frame(x = x, y = y2, Group = "Test 2")
    df <- rbind(df1, df2)
    
    
    set.seed(123) # for reproducibility
    
    # Generate a large number of random variates from each posterior distribution
    n_sims <- 1000000
    posterior1 <- rbeta(n_sims, conversions1 + 1, impressions1 - conversions1 + 1)
    posterior2 <- rbeta(n_sims, conversions2 + 1, impressions2 - conversions2 + 1)
    
    # Estimate the probability that Test 2 is better than Test 1
    prob_test2_better <- mean(posterior2 > posterior1)
    #
    print(paste("Probability that Test 2 is better than Test 1:", round(prob_test2_better, 3)))
    
    # Create the plot
    plot <- ggplot(df, aes(x = x, y = y, color = Group)) +
        geom_line() +
        theme_minimal() +
        labs(
            title = "Posterior distributions of conversion rates",
            subtitle = paste0("Probability that Test 2 is better than Test 1: ", round(prob_test2_better*100, 2),"%"),
            y = "Density",
            x = "Conversion rate") +
        geom_vline(data = data.frame(x = credible_interval1, Group = "Test 1"), aes(xintercept = x, color = Group), linetype = "dashed") +
        geom_vline(data = data.frame(x = credible_interval2, Group = "Test 2"), aes(xintercept = x, color = Group), linetype = "dashed") +
        annotate("text", x = max_x1 + .005, y = max(y1), 
                 label = paste0("Test 1 95% CI: [", round(credible_interval1[1], 3), ", ", round(credible_interval1[2], 3), "]"),
                 hjust = 1, color = "red") +
        annotate("text", x = max_x2 + .005, y = max(y2), 
                 label = paste0("Test 2 95% CI: [", round(credible_interval2[1], 3), ", ", round(credible_interval2[2], 3), "]"),
                 hjust = 1, color = "blue") +
        theme(legend.position = "bottom")
    
    # Return the plot
    return(plot)
}


impressions1 <- 1000
conversions1 <- 19
impressions2 <- 800
conversions2 <- 20

plot_beta(impressions1, conversions1, impressions2, conversions2)

# Simulate data for estimating probability of difference ----
set.seed(123) # for reproducibility

# Generate a large number of random variates from each posterior distribution
n_sims <- 1000000
posterior1 <- rbeta(n_sims, conversions1 + 1, impressions1 - conversions1 + 1)
posterior2 <- rbeta(n_sims, conversions2 + 1, impressions2 - conversions2 + 1)

# Estimate the probability that Test 2 is better than Test 1
prob_test2_better <- mean(posterior2 > posterior1)

print(paste("Probability that Test 2 is better than Test 1:", round(prob_test2_better, 3)))

# Perform a standard t-test ----

# Create data vectors representing successes and failures for each test
data1 <- c(rep(1, conversions1), rep(0, impressions1 - conversions1))
data2 <- c(rep(1, conversions2), rep(0, impressions2 - conversions2))

# Perform a two-sample t-test
t_test_result <- t.test(data1, data2)

print(t_test_result)

# Perform a proportions test
prop_test_result <- prop.test(c(conversions1, conversions2), c(impressions1, impressions2))

print(prop_test_result)

?prop.test()

library(ggplot2)
library(ggthemes)
library(ggpubr)

plot_conversion_rate_uplift <- 
    function(impressions1, 
             conversions1, 
             impressions2, 
             conversions2, 
             prob = 0.89) {
    
    # Generate posterior samples for the conversion rates of Test 1 and Test 2
    n_sims <- 100000
    posterior1 <- rbeta(n_sims, conversions1 + 1, impressions1 - conversions1 + 1)
    posterior2 <- rbeta(n_sims, conversions2 + 1, impressions2 - conversions2 + 1)
    
    # Calculate the difference in conversion rates
    diff_conversion_rate <- posterior2 - posterior1
    
    # Calculate the probability that Test 2 has a higher conversion rate than Test 1
    prob_test2_higher <- mean(diff_conversion_rate > 0)
    
    # Create a data frame for plotting
    df <- data.frame(Difference = diff_conversion_rate)
    
    # Create a density data frame for plotting
    dens <- density(diff_conversion_rate)
    dens_df <- data.frame(Difference = dens$x, Density = dens$y)
    
    # Compute the maximum density and corresponding difference
    max_density <- max(dens_df$Density)
    max_diff <- dens_df$Difference[which.max(dens_df$Density)]
    
    # Compute the 95% HPDI
    hpdi <- coda::HPDinterval(as.mcmc(diff_conversion_rate), prob = prob)
    
    # Plot the distribution of the difference and fill with different colors
    plot <- ggplot(df, aes(x = Difference)) +
        geom_line(stat = 'density', aes(y = after_stat(density)), colour = "black", linewidth = .5) +
        geom_ribbon(data = subset(dens_df, Difference >= 0), 
                    aes(ymax = Density, x = Difference), 
                    ymin = 0, fill = "#56B4E9", alpha = 0.6) +
        geom_ribbon(data = subset(dens_df, Difference < 0), 
                    aes(ymax = Density, x = Difference), 
                    ymin = 0, fill = "#E69F00", alpha = 0.6) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "dark grey") +
        geom_vline(xintercept = max_diff, linetype = "dashed", color = "dark grey") +
        geom_vline(xintercept = hpdi[1], linetype = "dashed", color = "light grey") +
        geom_vline(xintercept = hpdi[2], linetype = "dashed", color = "light grey") +
        annotate("text", x = 0.025, y = max(dens_df$Density) - 20,
                 label = paste0("P(Test > Base) = ", round(prob_test2_higher * 100, 1), "%"),
                 color = "black") +
        annotate("text", x = max_diff, y = max_density + 2, 
                 label = paste0("Most probable uplift: ", round(max_diff * 100, 1), "%"), 
                 color = "black") +
        annotate("text", x = mean(hpdi), y = max(dens_df$Density) + 5,
                 label = paste0(prob * 100,"% HPDI: [", round(hpdi[1], 2), ", ",
                                round(hpdi[2], 2), "]"),
                 color = "black") +
        labs(x = "Difference in Conversion Rate (Test - Base)", y = "Density",
             title = "Conversion Rate Uplift",
             subtitle = paste0("Probability of Test having a higher conversion rate than Base: ", 
                               round(prob_test2_higher * 100, 1), "%")) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"),
              text = element_text(size = 12),
              legend.position = "none")
    
    return(plot)
}


impressions1 <- 1000
conversions1 <- 19
impressions2 <- 800
conversions2 <- 20

plot_conversion_rate_uplift(impressions1 = 1000, 
                            conversions1 = 19, 
                            impressions2 = 800, 
                            conversions2 = 20)


# Compare with a z-test ----
conversions <- c(19, 20) # Number of successes (conversions)
impressions <- c(1000, 800) # Number of trials (impressions)

# Perform the test of proportions
(result <- prop.test(conversions, impressions))

