import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
from scipy.stats import binom, norm, beta, uniform
import seaborn as sns

# Function to select the prior function ----
def select_prior(prior_type="uniform", a=2, b=2):
    if prior_type == "uniform":
        def prior_function(pi_vals):
            return uniform.pdf(pi_vals)
    elif prior_type == "normal":
        def prior_function(pi_vals):
            mu = 0.5
            sigma = 0.1
            return norm.pdf(pi_vals, loc=mu, scale=sigma)
    elif prior_type == "beta":
        def prior_function(pi_vals):
            return beta.pdf(pi_vals, a=a, b=b)
    else:
        raise ValueError("Invalid prior type")

    # Preview prior distribution
    pi_vals = np.arange(0, 1, 0.01)
    prior = prior_function(pi_vals)
    data = pd.DataFrame({"pi": pi_vals, "prior": prior})
    g = sns.lineplot(data=data, x="pi", y="prior")
    g.set_title(f"Prior chances of team win probability")
    g.set_xlabel("Win Probability")
    g.set_ylabel("Prior Probability")

    return prior_function

# Example usage
uniform_prior = select_prior("uniform")
normal_prior = select_prior("normal")
beta_prior = select_prior("beta", a = 2, b = 5)

# Function to plot the posterior ----
def plot_posterior(games_played, games_won, prior):
    """
    Plots the posterior probability distribution for the win probability
    given the number of games played and the number of games won.

    Args:
        games_played (int): The number of games played.
        games_won (int): The number of games won.

    Returns:
        None.
    """
    n = games_played
    k = games_won
    
    pi_vals = np.arange(0, 1.01, 0.01)
    likelihoods = binom.pmf(k, n, pi_vals)
    prior = prior(pi_vals)
    
    unnormalized_posterior = likelihoods * prior
    posterior = unnormalized_posterior / np.sum(unnormalized_posterior)
    
    data = np.column_stack((pi_vals, posterior))
    
    data = pd.DataFrame({"pi": pi_vals, "posterior": posterior})
    
    plt.plot(data["pi"], data["posterior"])    
    plt.title(f"Chance of team win probability given {k} out of {n} games won")
    plt.xlabel("Win Probability")
    plt.ylabel("Chances")
    plt.show()

plot_posterior(games_played=7, games_won=1, prior=uniform_prior)

# Function to create posterior probability table ----
def display_posterior_table(games_played, prior):
    n = games_played
    pi_vals = np.arange(0.1, 1.0, 0.1)
    games_won = np.arange(0, games_played + 1)
    
    likelihoods = np.array([[binom.pmf(k, n, p) for p in pi_vals] for k in games_won])
    priors = np.array([prior(p) for p in pi_vals])
    unnormalized_posterior = likelihoods * priors
    posterior = unnormalized_posterior / np.sum(unnormalized_posterior, 
                                                axis=1, keepdims=True)
    
    data = pd.DataFrame(posterior, columns=[f"Pi = {p:.1f}" for p in pi_vals], 
                        index=games_won)
    data.index.name = "Games_Won"
    
    cmap = cm.Reds
    styled_table = (data
                    .style
                    .background_gradient(cmap=cmap, axis=1)
                    .highlight_max(axis=1, color="black")
                    .format("{:.3f}")
                    .set_caption("Posteriors for Team Win Probability \
                        Given Past Win/Loss Record"))
    
    return styled_table

display_posterior_table(games_played=8, prior=beta_prior)


