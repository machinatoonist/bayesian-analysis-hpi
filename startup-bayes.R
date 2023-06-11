
library(MASS)
## be careful to load dplyr after MASS
library(dplyr)
library(tidyr)
library(purrr)
library(extraDistr)
library(ggplot2)
library(loo)
library(bridgesampling)
library(brms)
library(bayesplot)
library(tictoc)
library(hypr)
library(bcogsci)
library(lme4)
library(rstan)
library(cmdstanr) # This package is optional, see https://mc-stan.org/cmdstanr/
library(SBC) # This package is optional, see https://hyunjimoon.github.io/SBC/
library(SHELF)
library(rootSolve)

## Save compiled models:
rstan_options(auto_write = FALSE)
## Parallelize the chains using all the cores:
options(mc.cores = parallel::detectCores())
# To solve some conflicts between packages
select <- dplyr::select
extract <- rstan::extract

# Generate random simulated number of successes
# 20 experiments with a sample size of 10
dat <- tibble(trial = rbinom(10, n = 2000, prob = 0.2))

# set number of trials and probability of success
trials <- 2000
prob_success <- 0.5

# generate binomial random variable
binomial_data <- rbinom(trials, 10, prob_success)

# create dataframe
data_frame <- tibble(trial = binomial_data)

# plot histogram
ggplot(data_frame, aes(trial)) +
    geom_histogram(bins = 30, position = 'identity') +
    ggtitle("Histogram of Binomially Distributed Random Variable") +
    xlab("Number of Successes") +
    ylab("Frequency")

# calculate the probability of a particular outcome
# Define parameters for binomial distribution
n <- 10 # number of trials
p <- 0.5 # probability of success

# Use a loop to compute probability of outcomes from 0 to 10 successes
for (x in 0:10) {
    # Compute probability of x successes using dbinom function
    prob <- dbinom(x, n, p)
    # Print result
    cat(x, " successes: ", prob, "\n")
}
