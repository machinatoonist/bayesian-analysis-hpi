# Setup: Marketing conducted a marketing test which we will call A.
# They sent out 16 emails got 6 subscriptions.
# What should we expect the signups to be on a large scale
num_pos <- 5
data_n <- 50
num_pos/data_n

# Number of draws from the prior
n_draws <- 100000

prior_rate <- runif(n_draws, 0, 1)
hist(prior_rate, xlim = c(0,1))

# Define the generative model
gen_model <- function(rate) {
    subscribers <- rbinom(1, size = data_n, prob = rate)
    subscribers
}

# Simulating the data
subscribers <- rep(NA, n_draws)

for(i in 1:n_draws) {
    subscribers[i] <- gen_model(prior_rate[i])
}

# Filtering out those parameter values that don't result in the
# data we observed
post_rate <- prior_rate[subscribers == num_pos]

# Check there are enough examples left
length(post_rate)

hist(post_rate, xlim = c(0, 1))

mean(post_rate)

quantile(post_rate)


quantile(post_rate, c(0.025, 0.975))

# Probability method A is better than the existing method with a success rate of 0.2
sum(post_rate > 0.2)/length(post_rate)

# If method A was used on 100 people what would be the expected number of sign-ups?

signups <- rep(NA, length(post_rate))

for(i in 1:length(post_rate)) {
    signups[i] <- rbinom(n = 1, size = 100, prob = post_rate[i])
}

hist(signups, xlim = c(0, 100))
# rbinom is vectorised so we can write:
signups <- rbinom(length(post_rate), size = 100, prob = post_rate)

hist(signups, xlim = c(0, 100))

quantile(signups)

# What are the chances conversions will be less than 0.05?
sum(post_rate < 0.05)/length(post_rate)
