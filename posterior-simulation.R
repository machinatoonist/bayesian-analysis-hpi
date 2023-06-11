# From Bayes Rules Chapter 2
library(bayesrules)
library(tidyverse)
library(janitor)

# The example given is of Gary Kasporov's second match with Deep Blue where
# he won only 1 out of 6 games

# Define win probability
chess <- tibble(pi = c(0.2, 0.5, 0.8))

# Define prior model
prior <- c(0.1, 0.25, 0.65)

chess_sample <- sample_n(tbl = chess, size = 10000, replace = TRUE, weight = prior)

glimpse(chess_sample)
mean(chess_sample$pi)

# Simulate number of wins from 6 games
chess_sim <- chess_sample %>% 
    mutate(y = rbinom(10000, 6, pi))

chess_sim %>% glimpse()
mean(chess_sim$y)

chess_sim %>% 
    tabyl(pi) %>% 
    adorn_totals("row") 

chess_sim %>% 
    count(pi) %>% 
    mutate(percent = n/sum(n))

chess_sim %>% 
    ggplot(aes(x = y)) +
    stat_count(aes(y = after_stat(prop))) +
    facet_grid(~pi) +
    labs(title = "Simulated win outcomes for each underlying win probability pi"
             )

chess_sim %>% 
    ggplot(aes(x = y)) +
    stat_count(aes(y = after_stat(prop))) +
    facet_grid(~factor(pi, levels = c(0.2, 0.5, 0.8), 
                       labels = c(paste("\u03C0 == 0.2"), 
                                  paste("\u03C0 == 0.5"),
                                  paste("\u03C0 == 0.8")))) +
    labs(title = "Simulated win outcomes for each underlying win probability \u03C0")

# Focus on simulations with y = 1

win_one <- chess_sim %>% 
    filter(y == 1) 

win_one %>%
    tabyl(pi) %>% 
    adorn_totals()

win_one %>% 
    ggplot(aes(x = pi)) +
    geom_bar() +
    labs(title = "Posterior win probability \u03C0",
         x = "Win probability \u03C0")
    

ggplot(aes(x = y)) +
    stat_count(aes(y = after_stat(prop)))
