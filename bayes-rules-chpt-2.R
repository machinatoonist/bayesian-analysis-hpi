# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Import article data
data(fake_news)

fake_news %>% glimpse()

fake_news %>% 
  tabyl(type) %>% 
  adorn_totals("row")

# Tabulate exclamation usage and article type

fake_news %>%
    tabyl(title_has_excl) %>%
    adorn_totals()

fake_news %>% 
  tabyl(title_has_excl, type) %>% 
  adorn_totals("row")

# Conclusion: If the title has an exclamation mark there is a 16/18 chance it is fake

?tabyl()
?adorn_totals()
 
fake_news %>% glimpse()

fake_news %>%
    tabyl(title_has_excl, type) %>%
    adorn_totals("row")

# Define the prior probability of an article being fake
prior <- 0.5

# Load the fake_news dataset
data(fake_news)

# Calculate the prior probability of an article being fake
prior_f <- sum(fake_news$type == "fake") / nrow(fake_news)

# Calculate the likelihood of an article being fake or real given that it contains an exclamation mark
likelihood_f <- sum(fake_news$type == "fake" &
                    fake_news$title_has_excl == TRUE) /
                    sum(fake_news$title_has_excl == TRUE)
likelihood_r <- sum(fake_news$type == "real" &
                    fake_news$title_has_excl == TRUE) /
                    sum(fake_news$title_has_excl == TRUE)

# Calculate the posterior probability of an article being fake or real given that it contains an exclamation mark using Bayes' rule
posterior_f <- likelihood_f * prior_f /
                (likelihood_f * prior_f + likelihood_r * (1 - prior_f))

posterior_r <- likelihood_r * (1 - prior_f) /
                (likelihood_f * prior_f + likelihood_r * (1 - prior_f))

# Print the posterior probabilities
cat("Posterior probability of an article being fake given an exclamation mark: ", posterior_f, "\n")
cat("Posterior probability of an article being real given an exclamation mark: ", posterior_r, "\n")

# Load the fake_news dataset
data(fake_news)

# Calculate the conditional probability of using an exclamation mark given that the article is fake or real
p_excl_f <- sum(fake_news$type == "fake" &
                fake_news$title_has_excl == TRUE) /
                sum(fake_news$type == "fake")

p_excl_r <- sum(fake_news$type == "real" &
                fake_news$title_has_excl == TRUE) /
                sum(fake_news$type == "real")

# Print the conditional probabilities
cat("Conditional probability of using an exclamation mark given that the article is fake: ", p_excl_f, "\n")
cat("Conditional probability of using an exclamation mark given that the article is real: ", p_excl_r, "\n")


# Let B denote the event that an article is fake and ~B the event that it's not fake
# Let A denote the event that an article has an exclamation point in the title

sum(fake_news$title_has_excl == TRUE & fake_news$type == "fake")/sum(fake_news$type == "fake")

sum(fake_news$title_has_excl == FALSE & fake_news$type == "fake")/sum(fake_news$type == "fake")


# Posterior simulation
# Define the possible article type, real or face and their corresponding probabilities
article <- tibble(type = c("real", "fake"))
article

# Define the prior model
prior <- c(0.6, 0.4)

# Simulate 3 articles
set.seed(2023)
# The prior must have the same dimensions as the dataframe to be sampled
slice_sample(article, n = 10, weight_by = prior, replace = TRUE)

set.seed(84735)
article_sim <- slice_sample(article,
                            n = 10000,
                            weight_by = prior,
                            replace = TRUE)

ggplot(article_sim, aes(x = type)) +
    geom_bar()

article_sim %>%
    tabyl(type) %>%
    adorn_totals("row")

article_sim %>% glimpse()

# Simulate exclamation point usage among these 10,000 articles.
# Recall that
fake_news %>%
    tabyl(title_has_excl, type) %>%
    adorn_totals("row")

(p_excl_given_fake <- 16 / 60)
(p_excl_given_real <- 2 / 90)

article_sim <- article_sim %>%
    mutate(data_model = case_when(type == "fake" ~ p_excl_given_fake,
                                    type == "real" ~ p_excl_given_real))

# Simulate whether each article includes an exclamation point
# Define whether there are exclamation points
data <- c("no", "yes")

# Simulate exclamation point usage
set.seed(3)
article_sim <- article_sim %>% 
    mutate(row_num = dplyr::row_number()) %>%
    group_by(row_num) %>%
    mutate(usage = sample(data, size = 1, prob = c(1 - data_model, data_model)))

article_sim %>% glimpse()

article_sim %>% 
  tabyl(usage, type) %>%
  adorn_totals(c("col", "row"))


ggplot(article_sim, aes(x = type, fill = usage)) +
    geom_bar(position = "fill") +
    labs(title = "Exclamation point usage by news type",
    fill = "Uses '!' Point")

ggplot(article_sim, aes(x = usage)) +
    geom_bar() +
    labs(title = "Overall exclamation point usage")

# Simulated data now reflect the prior model of fake news and likelihood of exclamation point usage
# Use these to approximate the posterior probability that the latest article is fake that have an exclamation point

article_sim %>%
    filter(usage == "yes") %>%
    tabyl(type) %>%
    adorn_totals("row")

ggplot(article_sim, aes(x = type)) +
    geom_bar() +
    facet_wrap(~ usage) +
    labs(title = "If the article includes an exclamation point it most likely is fake") +
    theme_minimal()


# Soda versus pop ----
# Load the data
data(pop_vs_soda)

# Summarize pop use by region
pop_vs_soda %>%
  tabyl(pop, region) %>% 
  adorn_percentages("col") %>%
  mutate(cond_prob = rowSums(select(., midwest:west)))
