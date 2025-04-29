
### Objective: To generate informative priors to share with 
# respondents prior to them retaking the same survey and 
# us then comparing responses across rounds. 
# It is a 2 round survey of the same questions. We would expect
# some convergence with the second round. 
# This kind of iterative surveying is called the delphi approach

# My concerns:
# Am I handling confidence appropriately? For some reason I don't 
# think I am. I'm currently weighting it.

#At the bottom I have script to create density plots instead
# instead of the individual line plots, but I'm confused by the 
# random noise that I added to make this work.

# If I attempt to use a package that calls stan, this does not
# work. It always runs into an error while compiling.


# Here it is!
# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(bayesplot)
library(tidybayes)

# data
# rating agreement (agreement) with a statement on a continuous 0-5 scale with 5 being agree perfectly
# 0 being do not agree at all, and 2.5 being something like I'm not really sure
# confidence (confidence) is rated on a scale from 0-100 percent, so 0 - 1 effectively
round1_df <- data.frame(
  respondent_id = 1:12,
  agreement = c(2, 2.5, 3, 1, 4.55, 2.75, 4, 2.25, .75, 5, 4.1, 3.1),      # Agreement ratings (0-5)
  confidence = c(.45, .55, .73, .89, .35, .22, .91, .15, .95, 1, .58, .39)      # Confidence levels (0-100)
)

# Convert prior beliefs to beta distribution parameters
# First scale agreement to 0-1
#round1_df$agree_scaled <- round1_df$agree/5

# Calculate alpha and beta parameters based on mean and precision
# Higher confidence = higher concentration parameter
#concentration <- 10 * round1_df$conf  # Scale appropriately
#round1_df$alpha <- round1_df$agree_scaled * concentration
#round1_df$beta <- (1 - round1_df$agree_scaled) * concentration

# Generate density plot for group responses
group_density_plot <- ggplot(round1_df, aes(x = agreement, weight = confidence)) +
  geom_density(fill = "lightblue", alpha = 0.5, color = "darkblue") +
  labs(title = "Group Prior Distribution (Weighted by Confidence)",
       x = "Agreement Rating (0-5)",
       y = "Density") +
  theme_minimal()

# Function to add individual's score as red bar to group density
add_individual_to_group <- function(plot, individual_id) {
  individual_score <- round1_df$agreement[round1_df$respondent_id == individual_id]
  plot + 
    geom_vline(xintercept = round1_df$agreement, color = "grey", alpha = .8, linewidth = 1) +
    geom_vline(xintercept = individual_score, color = "red", linewidth = 1) +
    labs(subtitle = paste("Individual", individual_id, "score shown in red"))
}

# Example: Generate plot for individual #3 compared to group
individual3_vs_group <- add_individual_to_group(group_density_plot, 3)


# Create individual density plots 
# using confidence as kernel bandwidth modifier

# Function to create individual density with confidence weighting
individual_density_plots <- function() {
  plots_list <- list()
  
  for (id in 1:12) {
    # Extract individual data
    ind_data <- round1_df %>% filter(respondent_id == id)
    
    # Scale bandwidth based on confidence (higher confidence = narrower distribution)
    bandwidth <- 0.5 * (1 - ind_data$confidence) + 0.1
    
    # Create density data
    x_values <- seq(0, 5, length.out = 100)
    density_values <- dnorm(x_values, mean = ind_data$agreement, sd = bandwidth)
    
    # Create plot
    ind_plot <- ggplot() +
      geom_line(aes(x = x_values, y = density_values), color = "blue") + 
      geom_vline(xintercept = round1_df$agreement, color = "lightgrey") +
      geom_vline(xintercept = ind_data$agreement, color = "red") +
      labs(title = paste("Respondent", id),
           x = "Agreement Rating",
           y = "Density") +
      theme_minimal()
    
    plots_list[[id]] <- ind_plot
  }
  
  return(plots_list)
}

# Combine all individual plots with the group average
library(gridExtra)

all_densities <- c(list(group_density_plot), individual_density_plots())

grid.arrange(grobs = all_densities, ncol = 3)

# Create summary statistics to share with respondents
round1_summary <- round1_df %>%
  summarise(
    mean_agreement = weighted.mean(agreement, confidence),
    weighted_sd = sqrt(sum(confidence * (agreement - mean_agreement)^2) / sum(confidence)),
    min_agreement = min(agreement),
    max_agreement = max(agreement),
    median_agreement = median(agreement)
  )

# Generate visualization for respondents to review
review_plot <- ggplot(round1_df, aes(x = agreement, y = confidence/100)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_vline(xintercept = round1_summary$mean_agreement, color = "blue", linetype = "dashed") +
  labs(title = "Round 1 Responses",
       subtitle = paste("Mean agreement:", round(round1_summary$mean_agreement, 2)),
       x = "Agreement Rating (0-5)",
       y = "Confidence Level (0-1)") +
  theme_minimal()

review_plot

# Rescale data
scaled_agreements <- round1_df$agreement / 5

# Estimate Beta parameters from data (method of moments)
mean_scaled <- mean(scaled_agreements, na.rm = TRUE)
var_scaled <- var(scaled_agreements, na.rm = TRUE)

# Calculate alpha and beta parameters
alpha_estimate <- mean_scaled * ((mean_scaled * (1 - mean_scaled) / var_scaled) - 1)
beta_estimate <- (1 - mean_scaled) * ((mean_scaled * (1 - mean_scaled) / var_scaled) - 1)

# Now incorporate confidence
get_beta_params <- function(scaled_agreement, confidence) {
  # Base concentration (higher values = more certainty)
  base_concentration <- alpha_estimate + beta_estimate
  
  exponent <- 2
  # Scale concentration by confidence
  # usually you add some hyperparameter here instead of hard
  # coding .2. I could use one of Rs set up distributions like pmax or something
  scaled_conf <- pmax(round1_df$confidence, 0.1) # set a floor at .01
  adjusted_concentration <- base_concentration * scaled_conf^exponent
  
  #adjusted_concentration <- base_concentration * (confidence + 0.2)
  
  # Maintain same mean but adjust concentration
  alpha_adj <- scaled_agreement * adjusted_concentration
  beta_adj <- (1 - scaled_agreement) * adjusted_concentration
  
  return(list(alpha = alpha_adj, beta = beta_adj))
}

# Visualize individual distributions
x_seq <- seq(0, 1, length.out = 100)
plot_data <- data.frame()

for(i in 1:nrow(round1_df)) {
  params <- get_beta_params(scaled_agreements[i], round1_df$confidence[i])
  
  temp_data <- data.frame(
    x = x_seq,
    density = dbeta(x_seq, params$alpha, params$beta),
    respondent = round1_df$respondent_id[i]
  )
  
  plot_data <- rbind(plot_data, temp_data)
}

ggplot(plot_data, aes(x = x*5, y = density, color = factor(respondent))) +
  geom_line() +
  labs(title = "Individual Beta Distributions (Scaled by Confidence)",
       x = "Agreement Rating (0-5)",
       y = "Density") +
  theme_minimal()


### To swap out the geom_line for geom_density in the 
# individual_density_plots function use this code

## Function to create individual density with confidence weighting
#individual_density_plots <- function() {
#  plots_list <- list()

#  for (id in 1:12) {
# Extract individual data
#   ind_data <- round1_df %>% filter(respondent_id == id)

# Create a data frame with replicated values to influence the density
# Number of replications based on confidence (higher confidence = more weight)
#   n_reps <- round(100 * ind_data$confidence)
#   density_df <- data.frame(
#     agreement = rep(ind_data$agreement, max(n_reps, 10))
#   )

# Add some noise scaled by inverse of confidence
# Higher confidence = less spread
# this is where building in mcmc could help to not introduce noise
#   noise_sd <- 0.5 * (1 - ind_data$confidence) + 0.1
#   density_df$agreement <- density_df$agreement + rnorm(nrow(density_df), 0, noise_sd)

# Create plot
#  ind_plot <- ggplot(density_df, aes(x = agreement)) +
#    geom_density(fill = "blue", alpha = 0.3) + 
#    geom_vline(xintercept = round1_df$agreement, color = "lightgrey") +
#    geom_vline(xintercept = ind_data$agreement, color = "red") +
#    labs(title = paste("Respondent", id),
#         x = "Agreement Rating",
#        y = "Density") +
#    xlim(0, 5) +  # Keep consistent x-axis range
#   theme_minimal()

#  plots_list[[id]] <- ind_plot
# }

# return(plots_list)
#}

# Combine all individual plots with the group average
#library(gridExtra)

#all_densities <- c(list(group_density_plot), individual_density_plots())

#grid.arrange(grobs = all_densities, ncol = 3)
