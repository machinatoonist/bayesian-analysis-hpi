# Chapter 2 exercises
library(tictoc)
library(tidyverse)
library(ggrepel)

# Probability of rolling three six-sided dice and getting a value greater than 7

(total_ways <- 6^3)

count_favourable_outcomes <- function(n = 7) {
    
    favourable_ways <- 0
    for (die1 in 1:6) {
        for (die2 in 1:6) {
            for (die3 in 1:6) {
                sum_die <- die1 + die2 + die3
                if (sum_die > n ) favourable_ways = favourable_ways + 1
            }
        }
        
    }
    
    return(favourable_ways)
}

count_favourable_outcomes(n = 7)

(probability <- count_favourable_outcomes(n = 7)/total_ways)
1/total_ways

# Alternative method
all_combinations <- 
    crossing(
        die1 = 1:6,
        die2 = 1:6,
        die3 = 1:6) %>% 
    mutate(sum = die1 + die2 + die3)

filtered_cols <- all_combinations %>% filter(sum > 7)

(prob_7 <- nrow(filtered_cols) / nrow(all_combinations))

# Alternative 3
sum(apply(expand.grid(c(1:6), c(1:6), c(1:6)), 1, sum) > 7)

# Calculate the probability using generating functions

# Question 3
p_sox <- 30/5/(1 + 30/5)
p_sox
{
require(graphics)

x <- c(0,0,0,100,0,0,0)
y <- c(0,0,1, 2 ,1,0,0)/4
zapsmall(convolve(x, y))         #  *NOT* what you first thought.
zapsmall(convolve(x, y[3:5], type = "f")) # rather
x <- rnorm(50)
y <- rnorm(50)

# Circular convolution *has* this symmetry:
all.equal(convolve(x, y, conj = FALSE), rev(convolve(rev(y),x)))

n <- length(x <- -20:24)
y <- (x-10)^2/1000 + rnorm(x)/8

Han <- function(y) # Hanning
    convolve(y, c(1,2,1)/4, type = "filter")

plot(x, y, main = "Using  convolve(.) for Hanning filters")
lines(x[-c(1  , n)      ], Han(y), col = "red")
lines(x[-c(1:2, (n-1):n)], Han(Han(y)), lwd = 2, col = "dark blue")
}

library(tidyverse)
library(ggrepel)

estimate_dice_roll_chances <- function(num_dice = 12, 
                                       sum_greater_than = 35,
                                       num_sides = 6) {
    
    if (sum_greater_than >= num_dice*num_sides) {
        return(paste0("Reduce sum_greater_than below maximum possible: ", 
                      num_dice*num_sides))
    } else if (sum_greater_than < num_dice) {
        return(paste0("Increase sum_greater_than above the: ", num_dice - 1))
    } else {
    
    # Calculate the probability using convolve()
    # num_dice <- 12
    # num_sides <- 6
    # sum_greater_than <- 35
    
    # Generate the probability vector for a single die
    (single_die_prob <- rep(1/num_sides, num_sides))
    
    # Generate the probability vector for the sum of multiple dice rolls
    (dice_sum_prob <- Reduce(function(x, y) convolve(x, y, type = "open"), 
                             rep(list(single_die_prob), num_dice)))
    
    # Calculate the probability of the sum being greater than sum_greater_than
    # Start summing from (sum_greater_than - num_dice + 2) to take into account 
    # the minimum possible sum
    (favorable_prob <- sum(dice_sum_prob[(sum_greater_than - 
                                              num_dice + 2):length(dice_sum_prob)]))
    
    # Output the result
    paste0("Probability of getting a sum greater than ", sum_greater_than, 
           " when rolling ", num_dice, " ", num_sides,"-sided dice: ", round(favorable_prob, 4))
    
    barplot(dice_sum_prob, main = "Probability Distribution of Dice Sums", 
            xlab = "Sum", ylab = "Probability", names.arg = num_dice:(num_dice * num_sides))
    
    # Create a tibble for the dice sums and their probabilities
    dice_sum_prob_df <- tibble(
        sum = num_dice:(num_dice * num_sides),
        probability = dice_sum_prob
    )
    
    # Determine the sum with the maximum probability
    max_prob_sum <- dice_sum_prob_df$sum[which.max(dice_sum_prob_df$probability)]
    
    # Plot the probability distribution using ggplot2
    ggplot(dice_sum_prob_df, aes(x = sum, y = probability)) +
        geom_bar(aes(fill = sum > sum_greater_than), stat = "identity") +
        scale_fill_manual(values = c("steelblue", "red")) +
        geom_vline(xintercept = max_prob_sum, color = "black", 
                   linetype = "dashed", size = 0.7) +
        geom_vline(xintercept = sum_greater_than, color = "black", 
                   linetype = "dashed", size = 0.7) +
        labs(title = paste0("Probability Distribution of Dice Sums: ",num_dice, " dice"), 
             x = "Sum", 
             y = "Probability",
             fill = paste0("Dice sum > ", sum_greater_than)) +
        theme_minimal() +
        geom_label_repel(data = data.frame(x = c(max_prob_sum, sum_greater_than), 
                                           y = c(max(dice_sum_prob_df$probability), 
                                                 max(dice_sum_prob_df$probability)), 
                                           label = c("Max Likely Sum", "Target Sum")),
                         aes(x = x, y = y, label = label), size = 3.5, 
                         box.padding = unit(0.35, "lines"), color = "black") +
        annotate("text",
                 x = max(dice_sum_prob_df$sum)/2 + 0.5, 
                 y = max(dice_sum_prob_df$probability)/1.25, 
                 label = paste("Chance: ", round(favorable_prob*100, 1), "%"), 
                 size = 5, 
                 color = "black",
                 hjust = -.5)
    }
}

estimate_dice_roll_chances(num_dice = 5, sum_greater_than = 20)

estimate_dice_roll_chances(num_dice = 2, sum_greater_than = 12)
estimate_dice_roll_chances(num_dice = 5, sum_greater_than = 4)


# Alternative approach using Monte Carlo simulation
# Parameters
num_dice <- 3
num_sides <- 6
target_sum <- 7
num_simulations <- 10000

# Function to roll a die
roll_die <- function(sides) {
    sample(1:sides, 1)
}

# Function to roll multiple dice and sum the result
roll_dice <- function(num_dice, sides) {
    sum(replicate(num_dice, roll_die(sides)))
}

# Run simulations
results <- replicate(num_simulations, roll_dice(num_dice, num_sides))

# Calculate probability
favorable_outcomes <- sum(results > target_sum)
estimated_probability <- favorable_outcomes / num_simulations

# Output the result
estimated_probability

estimate_dice_roll_chances(num_dice = num_dice, sum_greater_than = target_sum)


# Alternative approach using the binomial distribution ----
# But this is incorrect because the probability for each count changes with 
# the number of dice
n = 5
k = 3
# Number of ways of choosing k from n
choose(n = n, k = k)

p_6 = 1/6
p_n6 = 5/6

# Probability of 3x 6s in 5 rolls
choose(n, k)*p_6^k*p_n6^(n-k)
rbinom(n, k, prob = p_6)
?rbinom()
dbinom(n, k, p_6)


num_dice <- 2
num_sides <- 6

# Initialize a probability vector with 0's
dice_sum_prob <- numeric(num_dice * num_sides - num_dice + 1)

# Calculate probabilities for each possible sum
for (sum in num_dice:(num_dice * num_sides)) {
    for (face in 1:num_sides) {
        if (face <= sum) {
            num_ways <- choose(num_dice, sum - face)
            prob <- (1/num_sides)^num_dice
            dice_sum_prob[sum - num_dice + 1] <- dice_sum_prob[sum - num_dice + 1] + num_ways * prob
        }
    }
}

dice_sum_prob

library(ggplot2)
library(tibble)
library(ggrepel)

estimate_dice_roll_chances_binomial <- function(num_dice = 12, 
                                                sum_greater_than = 35,
                                                num_sides = 6) {
    
    # Initialize a probability vector with 0's
    dice_sum_prob <- numeric(num_dice * num_sides - num_dice + 1)
    
    # Calculate probabilities for each possible sum
    for (sum in num_dice:(num_dice * num_sides)) {
        for (face in 1:num_sides) {
            if (face <= sum) {
                num_ways <- choose(num_dice, sum - face)
                prob <- (1/num_sides)^num_dice
                dice_sum_prob[sum - num_dice + 1] <- dice_sum_prob[sum - num_dice + 1] + num_ways * prob
            }
        }
    }
    
    # Create a tibble for the dice sums and their probabilities
    dice_sum_prob_df <- tibble(
        sum = num_dice:(num_dice * num_sides),
        probability = dice_sum_prob
    )
    
    # Determine the sum with the maximum probability
    max_prob_sum <- dice_sum_prob_df$sum[which.max(dice_sum_prob_df$probability)]
    
    # Plot the probability distribution using ggplot2
    g <- ggplot(dice_sum_prob_df, aes(x = sum, y = probability)) +
        geom_bar(stat = "identity") +
        labs(title = paste0("Probability Distribution of Dice Sums (Binomial): ",num_dice, " dice"), 
             x = "Sum", 
             y = "Probability") +
        geom_vline(xintercept = max_prob_sum, color = "red", linetype = "dashed", size = 0.7) +
        geom_label_repel(data = data.frame(x = max_prob_sum, 
                                           y = max(dice_sum_prob_df$probability), 
                                           label = "Max Likely Sum"),
                         aes(x = x, y = y, label = label), size = 3.5, 
                         box.padding = unit(0.35, "lines"), color = "black") +
        theme_minimal()
    
    print(g)
    
    return(dice_sum_prob_df)
}

estimate_dice_roll_chances_binomial(num_dice = 5, sum_greater_than = 20)

# Chapter 4
# 1: What are the paramters of the binomial distribution for the probability
# of rolling a 1 or a 20 on a 20-sided die if we roll the die 12 times
n = 12
k = 1 # only one because its 1 OR 20 not 1 and 20
p = 2/20

# 2: THere are 4 aces in a deck of 52 cards.  If you pull a card, return the card
# then reshuffle and pull a card again, how many ways can you pull just one ace
# five pulls
choose(n = 5, k = 1)

# 3: What is the probability of pulling five aces in 10 pulls?
choose(n = 10, k = 5)*(4/52)^5*(48/52)^(10-5)
1/2200

# 4: THere is a 1/5 chance of getting a job offer and you
# want to know your chances of getting at least 2 offers out of 7 interviews
p = 1/5
n = 7
k = 2
pbinom(q = k - 1, size = n, prob = p, lower.tail = FALSE)

# 5: 25 interviews with a 1/10 chance of a job offer increases your chances
pbinom(q = 1, size = 25, prob = 1/10, lower.tail = FALSE)

# beta function ----
# Perform statistical inference on observed data by determining which probabilities
# we might assign to an event and how strongly we believe in each one.
# A probability of probabilities.

integrate(function(p) dbeta(p, 14, 27), 0, 0.5)

# Plotting a Beta PDF in R

# Define the shape parameters for the Beta distribution
alpha <- 5
beta <- 1195

# Use the curve function to plot the Beta PDF
curve(dbeta(x, alpha, beta), 
      from = 0, 
      to = .01, 
      ylab = "Density", 
      xlab = "x",
      main = paste("Beta(", alpha, ",", beta, ") Distribution"))

# Add a grid for easier interpretation
grid()

# What are the chances the probability is greater than 0.005?
integrate(function(p) dbeta(p, alpha, beta), 0.005, 1)

## Exercises ----
# 1: A coin flipped 10 times gives 4 head and 6 tails
# What is the probability the coin will land on heads more than 60% of the time?

integrate(function(p) dbeta(p, 4, 6), 0.6, 1)

integrate(function(p) dbeta(p, 9, 11), 0.45, 0.55)

alpha <- 109
beta <- 110
integrate(function(p) dbeta(p, alpha, beta), 0.45, 0.55)

curve(dbeta(x, alpha, beta), 
      from = 0, 
      to = 1, 
      ylab = "Density", 
      xlab = "x",
      main = paste("Beta(", alpha, ",", beta, ") Distribution"))

# Add a grid for easier interpretation
grid()

# Chapter 9
?dbeta()
what_are_my_chances <- function(successes, failures, min = .4, max = 0.6) {
    
    alpha = successes
    beta = failures
    
    curve(dbeta(x, alpha, beta), 
          from = 0, 
          to = 1, 
          ylab = "Density", 
          xlab = "x",
          main = paste("Beta(", alpha, ",", beta, ") Distribution"))
    
    grid()
    
    chance = integrate(function(p) dbeta(p, alpha, beta), min, max)
    
    return(paste0("There is a ", round(chance$value * 100, 1), 
                  "% chance the true rate is between ", min, " and ", max))
}

what_are_my_chances(successes = 5, failures = 3)

what_are_my_chances(6, 1)

define_my_prior <- function(min = 0.4, max = 0.6, desired_chance = 0.95) {
    successes <- 0
    failures <- 0
    chance <- 0
    
    while (chance < desired_chance) {
        successes <- successes + 1
        failures <- failures + 1
        
        alpha <- successes
        beta <- failures
        
        chance <- integrate(function(p) dbeta(p, alpha, beta), min, max)$value
    }
    
    curve(dbeta(x, alpha, beta), 
          from = 0, 
          to = 1, 
          ylab = "Density", 
          xlab = "x",
          main = paste("Beta(", alpha, ",", beta, ") Distribution"))
    grid()
    
    return(paste0("To achieve a ", round(chance * 100, 1), "% chance of success between ", min, " and ", max, 
                  ", you need ", successes, " successes and ", failures, " failures."))
}

define_my_prior(min = 0.1, max = 1, desired_chance = 0.95)


library(ggplot2)

define_my_prior <- function(min = 0.4, max = 0.6, desired_chance = 0.95) {
    successes <- 0
    failures <- 0
    chance <- 0
    
    library(ggplot2)
    
    while (chance < desired_chance) {
        successes <- successes + 1
        failures <- failures + 1
        
        alpha <- successes
        beta <- failures
        
        chance <- integrate(function(p) dbeta(p, alpha, beta), min, max)$value
    }
    
    x <- seq(0, 1, length.out = 1000)
    y <- dbeta(x, alpha, beta)
    df <- data.frame(x, y)
    
    plot <- ggplot(df, aes(x, y)) +
        geom_line(color = "black", size = 0.7) +
        geom_area(data = subset(df, x >= min & x <= max), fill = "dodger blue", alpha = 0.5) +
        labs(x = "x", y = "Density",
             title = paste0("Prior Distribution: Beta(", alpha, ",", beta, ")"),
             subtitle = paste0("To achieve a ", round(chance * 100, 1), "% chance of success between ", min, " and ", max, 
                               ", you need ", successes, " successes and ", failures, " failures.")) +
        theme_minimal()
    
    print(plot)
    
    return(paste0("To achieve a ", round(chance * 100, 1), "% chance of success between ", min, " and ", max, 
                  ", you need ", successes, " successes and ", failures, " failures.\n\n"))
}

define_my_prior <- function(successes = 0, failures = 0, min = 0.4, max = 0.6, desired_chance = 0.95) {
    chance <- 0
    
    library(ggplot2)
    
    while (chance < desired_chance) {
        successes <- successes + 1
        failures <- failures + 1
        
        alpha <- successes
        beta <- failures
        
        chance <- integrate(function(p) dbeta(p, alpha, beta), min, max)$value
    }
    
    x <- seq(0, 1, length.out = 1000)
    y <- dbeta(x, alpha, beta)
    df <- data.frame(x, y)
    
    plot <- ggplot(df, aes(x, y)) +
        geom_line(color = "black", size = 0.7) +
        geom_area(data = subset(df, x >= min & x <= max), fill = "dodger blue", alpha = 0.5) +
        labs(x = "x", y = "Density",
             title = paste0("Prior Distribution: Beta(", alpha, ",", beta, ")"),
             subtitle = paste0("To achieve a ", round(chance * 100, 1), "% chance of success between ", min, " and ", max, 
                               ", you need ", successes, " successes and ", failures, " failures.")) +
        theme_minimal()
    
    print(plot)
    
    return(paste0("To achieve a ", round(chance * 100, 1), "% chance of success between ", min, " and ", max, 
                  ", you need ", successes, " successes and ", failures, " failures.\n\n"))
}


# Example 1
result1 <- define_my_prior(min = 0.4, max = 0.6, desired_chance = 0.5)
print(result1)

# Example 2
result2 <- define_my_prior(min = 0.4, max = 0.6, desired_chance = 0.95)
print(result2)

result3 <- define_my_prior(min = 0.2, max = 1, desired_chance = 0.95)

define_my_prior(successes = 20, 
                failures = 1, 
                min = 0.4, 
                max = 0.6, 
                desired_chance = 0.95)

define_my_prior(successes = 2, failures = 20, 0.4, 0.6, desired_chance = 0.95)


define_my_prior(successes)

library(ggplot2)
library(ggplot2)

library(ggplot2)

minimum_failures_required <- function(successes = 0, threshold, max_iterations = 10000) {
    failures_0 <- 55
    failures <- 55
    successes <- 55 + successes

    for (i in 1:max_iterations) {
        failures <- failures + 1
        
        alpha <- successes
        beta <- failures
        
        chance <- integrate(function(p) dbeta(p, alpha, beta), 0.4, 0.6)$value
        
        if (chance < threshold) {
            break
        }
    }
    
    plot_df <- data.frame(x = seq(0, 1, length.out = 1000), 
                          y = dbeta(seq(0, 1, length.out = 1000), alpha, beta))
    
    print(ggplot(plot_df, aes(x, y)) +
        geom_line(color = "blue", size = 1) +
        geom_area(data = subset(plot_df, x >= 0.4), fill = "lightblue", alpha = 0.5) +
        labs(x = "x", y = "Density", 
             title = paste0("Beta(", alpha, ",", beta, ") Distribution")) +
        theme_minimal())
    
    return(paste0("There is less than a ", round(threshold * 100, 1), 
                  "% chance of an even chance of success after ", failures - failures_0, " failures."))
}


minimum_failures_required(
    successes = 0,
    threshold = 0.5,
    max_iterations = 1000)

successes <- 0
alpha <- 55
beta <- 55
threshold <- 0.5
max_iterations = 10000

for (i in 1:max_iterations) {
    
    chance <- integrate(function(p) dbeta(p, alpha, beta), 0.4, 0.6)$value
    
    if (chance < threshold) {
        break
    }
    alpha <- alpha + 1
}

print(alpha)

# Chapter 11 ----

x <- seq(1, 10, 1)

mean(x)
var(x)
sd(x)
x
# basic variance
sum((x - mean(x))^2)/length(x)

# basic sdev
(sum((x - mean(x))^2)/length(x))^0.5

# Chapter 12 - The Normal Distribution ----
# Find the probability that a fuse will burn through 

trials <- c(19, 22, 20, 19, 23)

(mean_time <- mean(trials))
(stdev <- sd(trials))

(stdev_long <- (sum((trials - mean_time)^2)/(length(trials)))^0.5)

integrate(function(x) dnorm(x, mean_time, stdev_long), lower = 10, upper = 18)

# Question 1
integrate(function(x) dnorm(x, 0, 1), lower = 5, upper = 100)

# Question 2
temps <- c(100, 99.8, 101, 100.5, 99.7)
mean_temp <- mean(temps)
temp_sd <- sd(temps)
?sd()
integrate(function(x) dnorm(x, mean_temp, temp_sd), lower = 100.4, upper = 100.4 + 3*temp_sd)

# Question 3
timings <- c(2.5, 3, 3.5, 4.2)
mean_time <- mean(timings)
time_sd <- sd(timings)
distance <- 0.5 * 9.8 * timings^2
distance

(mean_dist <- mean(distance))
(sd_dist <- sd(distance))
integrate(function(x) dnorm(x, mean_dist, sd_dist), lower = 500, upper = 500 + 8*sd_dist)

integrate(function(x) dnorm(x, mean_dist, sd_dist), lower = -8*sd_dist, upper = 0 )


# Chapter 13 ----

xs <- seq(0.005, 0.01, by = 0.00001)
xs.all <- seq(0, 2, by = 0.00001)
plot(xs, dbeta(xs, 300, 40000 - 300), type = 'l', lwd = 3,
     ylab = "density",
     xlab = "probability of subscription",
     main = "PDF Beta(300, 39700)")

plot(xs, pbeta(xs, 300, 40000 - 300), type = 'l', lwd = 3,
     ylab = "Probability",
     xlab = "probability of subscription",
     main = "CDF Beta(300, 39700)")

ys <- seq(0.001, 0.999, by = 0.001)

plot(ys, qbeta(ys, 300, 40000 - 300), type = 'l', lwd = 3,
     ylab = "Probability of subscription",
     xlab = "probability",
     main = "Quantile function of Beta(300, 39700)")

snowfall <- c(7.8, 9.4, 10.0, 7.9, 9.4, 7.0, 7.0, 7.1, 8.9, 7.4)

(mean_snowfall <- mean(snowfall))
(stdev_snowfall <- sd(snowfall))

sn <- seq(0, 20, 0.1)

plot(ys, qnorm(ys, mean_snowfall, stdev_snowfall))

qnorm(.0005, mean_snowfall, stdev_snowfall)
qnorm(.9995, mean_snowfall, stdev_snowfall)

.9995 - 0.0005

# 3 
# Given the following success rate
total_houses <- 30
successes <- 10

# She will visit 40 more houses today
# What is the 95% confidence interval for how many candy bars she will sell
# for the rest of the day?

# Cumulative probability function pbeta()
# Probability density function dbeta()
# Inverse cumulative distribution function: qbeta()

# lower bound
# qbeta(p = 0.025, shape1 = successes, shape2 = total_houses - successes)
qbeta(0.025, successes, total_houses - successes)*40

qbeta(0.975, successes, total_houses - successes)*40

# Can also calculate the 95% confidence interval using the binomial distribution
qbinom(.025, 40, 1/3)
qbinom(.975, 40, 1/3)

plot(ys, qbeta(ys, 10, 30))

# Chapter 14 - Prior probabilities ----

# A: Weak belief the coin is biased to land on head 70% of the time
qbeta(.5, 7, 3)

plot(ys, dbeta(ys, 7, 3))

# B: Person strongly believes the coin is fair
qbeta(.5, 100, 100)
plot(ys, dbeta(ys, 100, 100))

# C: Person strongly believes the coin is biased
qbeta(.5, 70, 30)
plot(ys, dbeta(ys, 70, 30))

# 2
# flip the coin 20 more times and get 9 head and 11 tails

# Posterior beliefs
# A:
prior_successes <- 7
prior_failures <- 3
successes <- prior_successes + 9
failures <- prior_failures + 11
plot(ys, dbeta(ys, prior_successes, prior_failures))
plot(ys, dbeta(ys, successes, failures))

qbeta(0.025, successes, failures)
qbeta(0.975, successes, failures)

posterior_beliefs <- function(prior_successes, 
                              prior_failures, 
                              updated_successes,
                              updated_failures) {
    
    ys <- seq(0.001, 0.999, by = 0.001)
    
    successes <- prior_successes + updated_successes
    failures <- prior_failures + updated_failures
    plot(ys, dbeta(ys, prior_successes, prior_failures),
         ylab = "Probability of fair coin",
         xlab = "probability",
         main = "Prior Density function")
    plot(ys, dbeta(ys, successes, failures),
         ylab = "Probability of fair coin",
         xlab = "probability",
         main = "Posterior Density function")
    
    print(qbeta(0.025, successes, failures))
    print(qbeta(0.975, successes, failures))
    
    return("Updated Priors!")
    
}

posterior_beliefs(7, 3, 9, 11)


# B:

posterior_beliefs(prior_successes = 1000, 
                  prior_failures = 1000, 
                  updated_successes = 9, 
                  updated_failures = 11)

# C: 
posterior_beliefs(70, 30, 9, 11)


# Chapter 15 - Hypothesis testing ----

# 1 Does removing an image from an email increase click through rates?
n_trials <- 50000
# Weak prior
prior_alpha <- 3
prior_beta <- 7

# Strong prior
prior_alpha <- 300
prior_beta <- 700


# Data to generate the likelihood
# A = results from emails with image
a_successes <- 36
a_failures <- 114
# B = results from emails with no image
b_successes <- 50
b_failures <- 100

set.seed(2023)
# Collect samples from each variant
a_samples <- rbeta(n_trials, a_successes, a_failures)
b_samples <- rbeta(n_trials, b_successes, b_failures)

mean(b_samples - a_samples)
sum(b_samples > a_samples)/n_trials
mean(b_samples/a_samples)

plot(ecdf(b_samples/a_samples))

uplift <- (b_samples - a_samples)/a_samples
cdist <- ecdf(uplift)

plot(cdist, verticals = TRUE)

# Unique data values
knots(cdist)
summary(cdist)
summary.stepfun(cdist)

ls(environment(cdist))

plot(cdist, verticals = TRUE)

plot(cdist, col.hor = "red", add = TRUE)
# abline(v = knots(cdist), lty = 2, col = "gray70")