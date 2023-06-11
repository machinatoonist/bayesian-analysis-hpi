library(ggplot2)
library(plotly)
library(tidyverse)
# 
# sample_uplift <- function(prior_alpha, 
#                           prior_beta, 
#                           a_successes, 
#                           a_total, 
#                           b_successes, 
#                           b_total,
#                           n_trials = 50000) {
#     
#     a_failures <- a_total - a_successes
#     b_failures <- b_total - b_successes
#     
#     set.seed(2023)
#     
#     a_samples <- rbeta(n_trials, a_successes, a_failures)
#     b_samples <- rbeta(n_trials, b_successes, b_failures)
#     
#     uplift <- (b_samples - a_samples)/a_samples
# 
#     df <- tibble(uplift = sort(uplift))  
#     
#     df <- df %>% mutate(ECDF = seq_along(df$uplift)/length(df$uplift),
#                hover_text = paste0("Uplift: ", 
#                                    round(100*uplift, 1), 
#                                    "%<br>Chance: ", 
#                                    round(100*(1 - ECDF), 1), "%")) %>% 
#         slice_sample(prop = 0.1) %>% 
#         arrange(uplift)
#     
#     return(df)
# }
# 
# 
# plot_uplift_ecdf <- function(df,
#                              hdi_low = 0.25,
#                              hdi_high = 0.75) {
# 
#     hdi <- quantile(df$uplift, probs = c(hdi_low, hdi_high))
#     
#     df_hdi <- df %>% filter(uplift >= hdi[1], uplift <= hdi[2])
#     
#     gg <- ggplot(data = df, aes(x = uplift, y = ECDF)) +
#         geom_line(color = "black") +
#         geom_point(aes(text = hover_text), size = 0.01, alpha = 0.4) +
#         geom_line(data = df_hdi, aes(x = uplift, y = ECDF), color = "dodgerblue") +
#         geom_area(data = df_hdi, aes(x = uplift, y = ECDF), fill = "dodgerblue", alpha = 0.4) +
#         xlim(-0.2, 1) +
#         labs(x = "Uplift", 
#              y = "Empirical Cumulative Distribution Function",
#              title = "What is the Chance of That Conversion Rate Uplift?",
#              subtitle = "Interpreting the results of A/B tests for Managers can be tricky") +
#         geom_vline(xintercept = median(df$uplift), 
#                    linetype = "dashed", 
#                    color = "red") +
#         annotate("text", 
#                  x = (median(df$uplift))*1.2, 
#                  y = 0.4, 
#                  label = paste("Median:", round(median(df$uplift), 2)), 
#                  color = "black") +
#         annotate("text", 
#                  x = hdi[1], 
#                  y = 0.05, 
#                  label = paste("HDI:", round(hdi[1], 2)), 
#                  hjust = -0.5, color = "black") +
#         annotate("text", 
#                  x = hdi[2], 
#                  y = 0.05, 
#                  label = paste("HDI:", round(hdi[2], 2)), 
#                  hjust = 1.5, color = "black") +
#         theme_minimal()
#     
#         p <- ggplotly(gg, tooltip = c("text") )
#         
#         return(p)
#     
# }


sample_uplift <- function(prior_alpha, 
                          prior_beta, 
                          a_successes, 
                          a_total, 
                          b_successes, 
                          b_total,
                          n_trials = 50000) {
    
    prior_alpha <- prior_alpha + 1
    prior_beta <- prior_beta + 1
    
    a_failures <- a_total - a_successes
    b_failures <- b_total - b_successes
    
    # posterior parameters
    posterior_alpha_a = prior_alpha + a_successes
    posterior_beta_a = prior_beta + a_failures
    posterior_alpha_b = prior_alpha + b_successes
    posterior_beta_b = prior_beta + b_failures
    
    set.seed(2023)
    
    # sample from the prior distribution
    prior_samples_a <- rbeta(n_trials, prior_alpha, prior_beta)
    prior_samples_b <- rbeta(n_trials, prior_alpha, prior_beta)
    prior_uplift <- (prior_samples_b - prior_samples_a)/prior_samples_a
    
    # sample from the posterior distribution
    posterior_samples_a <- rbeta(n_trials, posterior_alpha_a, posterior_beta_a)
    posterior_samples_b <- rbeta(n_trials, posterior_alpha_b, posterior_beta_b)
    posterior_uplift <- (posterior_samples_b - posterior_samples_a)/posterior_samples_a
    
    df_prior <- tibble(uplift = sort(prior_uplift),
                       ECDF = seq_along(prior_uplift)/length(prior_uplift),
                       hover_text = paste0("Uplift: ", 
                                           round(100*sort(prior_uplift), 1), 
                                           "%<br>Chance: ", 
                                           round(100*(1 - ECDF), 1), "%"),
                       Distribution = "Prior")
    
    df_posterior <- tibble(uplift = sort(posterior_uplift),
                           ECDF = seq_along(posterior_uplift)/length(posterior_uplift),
                           hover_text = paste0("Uplift: ", 
                                               round(100*sort(posterior_uplift), 1), 
                                               "%<br>Chance: ", 
                                               round(100*(1 - ECDF), 1), "%"),
                           Distribution = "Posterior")
    
    
    df <- bind_rows(df_prior, df_posterior)
    
    return(df)
}

plot_uplift_ecdf <- function(df,
                             hdi_low = 0.25,
                             hdi_high = 0.75) {
    
    post_df <- df %>% filter(Distribution == "Posterior")
    
    hdi <- quantile(post_df$uplift, probs = c(hdi_low, hdi_high))
    
    df_hdi <- post_df %>% filter(uplift >= hdi[1], uplift <= hdi[2])
    
    xlim_min = min(post_df$uplift) - abs(min(post_df$uplift) * 0.05)
    xlim_max = max(post_df$uplift) + abs(max(post_df$uplift) * 0.05)
    
    gg <- ggplot(data = df, aes(x = uplift, y = ECDF, color = Distribution)) +
        geom_line() +
        geom_point(data = post_df, aes(text = hover_text), size = 0.01, alpha = 0.4) +
        geom_line(data = df_hdi, aes(x = uplift, y = ECDF), color = "dodgerblue") +
        geom_area(data = df_hdi, aes(x = uplift, y = ECDF), fill = "dodgerblue", alpha = 0.4) +
        xlim(xlim_min, xlim_max) +
        # labs(x = "Uplift", 
        #      y = "Empirical Cumulative Distribution Function",
        #      title = "What is the Chance of That Conversion Rate Uplift?",
        #      subtitle = "Interpreting the results of A/B tests for Managers can be tricky") +
        geom_vline(xintercept = median(df$uplift[df$Distribution == "Posterior"]), 
                   linetype = "dashed", 
                   color = "red") +
        annotate("text", 
                 x = median(df$uplift[df$Distribution == "Posterior"])*0.5, 
                 y = 0.5, 
                 label = paste("Median:", 
                               round(median(df$uplift[df$Distribution == "Posterior"]), 2)), 
                 color = "black") +
        theme(legend.position = "none") +
        theme_minimal() +
        scale_color_manual(values = c("Prior" = "grey", "Posterior" = "black"))
    
    p <- ggplotly(gg, tooltip = c("text")) %>% 
        layout(
            title = list(text = "What is the Chance of That Conversion Rate Uplift?", font = list(size = 30)),
            legend = list(orientation = "h", 
                          x = 0.5, 
                          y = -0.2, 
                          xanchor = "center",
                          font = list(size = 20)),
            xaxis = list(title = "Uplift", titlefont = list(size = 20), 
                         tickfont = list(size = 16)),
            yaxis = list(title = "Empirical Cumulative Distribution Function", 
                         titlefont = list(size = 20), tickfont = list(size = 16)),
            hoverlabel = list(font = list(size = 24)),
            margin = list(t = 100)
            
            # annotations = list(
            #     list(
            #         x = median_norm,
            #         y = 0.5,
            #         text = paste("Median:", 
            #                      round(median_val, 2)),
            #         font = list(size = 20),
            #         showarrow = TRUE,
            #         arrowhead = 1,
            #         ax = 20,
            #         ay = -30
            #     )
            # )
        )
    
    
    return(p)
}
# 
# plot_uplift_ecdf <- function(df,
#                              hdi_low = 0.25,
#                              hdi_high = 0.75) {
#     
#     post_df <- df %>% filter(Distribution == "Posterior")
#     
#     hdi <- quantile(post_df$uplift, probs = c(hdi_low, hdi_high))
#     
#     df_hdi <- post_df %>% filter(uplift >= hdi[1], uplift <= hdi[2])
#     
#     xlim_min = min(post_df$uplift) - abs(min(post_df$uplift) * 0.05)
#     xlim_max = max(post_df$uplift) + abs(max(post_df$uplift) * 0.05)
#     
#     median_val <- median(df$uplift[df$Distribution == "Posterior"])
#     
#     # Normalize the median value
#     median_norm = (median_val - xlim_min) / (xlim_max - xlim_min)
#     
#     gg <- ggplot(data = df, aes(x = uplift, y = ECDF, color = Distribution)) +
#         geom_line() +
#         geom_point(data = post_df, aes(text = hover_text), size = 0.01, alpha = 0.4) +
#         geom_line(data = df_hdi, aes(x = uplift, y = ECDF), color = "dodgerblue") +
#         geom_area(data = df_hdi, aes(x = uplift, y = ECDF), fill = "dodgerblue", alpha = 0.4) +
#         xlim(xlim_min, xlim_max) +
#         labs(x = "Uplift", y = "Empirical Cumulative Distribution Function",
#              title = "What is the Chance of That Conversion Rate Uplift?",
#              subtitle = "Interpreting the results of A/B tests for Managers can be tricky") +
#         geom_vline(xintercept = median_val, 
#                    linetype = "dashed", 
#                    color = "red") +
#         annotate("text", 
#                  x = median_val, 
#                  y = 0.75, 
#                  label = paste("Median:", 
#                                round(median_val, 2)), 
#                  color = "black", 
#                  size = 5, 
#                  hjust = -0.1) +
#         theme(legend.position = "none",
#               text = element_text(size = 20), 
#               axis.title = element_text(size = 20),
#               plot.title = element_text(size = 25),
#               plot.subtitle = element_text(size = 18)) +
#         theme_minimal() +
#         scale_color_manual(values = c("Prior" = "grey", "Posterior" = "black"))
#     
#     p <- ggplotly(gg, tooltip = c("text")) %>% 
#         layout(legend = list(orientation = "h", 
#                              x = 0.5, 
#                              y = -0.2, 
#                              xanchor = "center"),
#                annotations = list(
#                    x = median_norm,
#                    y = 0.5,
#                    text = paste("Median:", 
#                                 round(median_val, 2)),
#                    font = list(size = 20),
#                    showarrow = TRUE,
#                    arrowhead = 1
#                ),
#                xaxis = list(titlefont = list(size = 20)),
#                yaxis = list(titlefont = list(size = 20)))
#     
#     return(p)
# }



# df <- sample_uplift(
#     # Strength of prior belief
#     prior_alpha = 0,
#     prior_beta = 0,
#     # A = results from emails with image
#     a_successes = 36,
#     a_total = 114 + 36,
#     # B = results from emails with no image
#     b_successes = 50,
#     b_total = 100 + 50
# )

# plot_uplift_ecdf(df = df, 
#                  hdi_low = .25, 
#                  hdi_high = .75)



