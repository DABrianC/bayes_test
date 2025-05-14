

round1_df <- data.frame(
  respondent_id = 1:12,
  agreement = c(2, 2.5, 3, 1, 4.55, 2.75, 4, 2.25, .75, 5, 4.1, 3.1),      # Agreement ratings (0-5)
  confidence = c(.45, .55, .73, .89, .35, .22, .91, .15, .95, 1, .58, .39)      # Confidence levels (0-100)
)

group_x <- seq(0,5, length.out = 1000)
group_mean <- mean(round1_df$agreement)
group_sd <- mean(0.5 * (1 - round1_df$confidence) + 0.1)
group_density <- dnorm(x = group_x
                       , mean = group_mean
                       , sd = group_sd)
  
group_df <- data.frame(group_x
                       , group_density)

ind_x <- seq(0, 5, length.out = 1000)
ind_mean <- mean(round1_df$agreement[round1_df$respondent_id==10])
ind_sd <- mean(0.5 * (1 - round1_df$confidence[round1_df$respondent_id==10]) + 0.1)

ind_density <- dnorm(x = ind_x
                     , mean = ind_mean
                     , sd = ind_sd)

ind_df <- data.frame(ind_x, ind_density)

group_plot <- ggplot(group_df) +
  geom_density(aes(x = group_x, weight = group_density), fill = "blue", color = "blue", alpha = .4) +
  geom_density(data = ind_df, aes(x = ind_x, weight = ind_density)
               , fill = "red", color = "red", alpha = .6) +
  geom_vline(xintercept = mean(round1_df$agreement), color = "blue")+
  geom_vline(xintercept = round1_df$agreement[round1_df$respondent_id==10], color = "red")+
  theme_minimal()
  
group_plot


individual_density_plots <- function() {
  plots_list <- list()
  
  # First, create a data frame for all individual density curves
  all_densities <- data.frame()
  
  for (id in 1:12) {
    # Extract individual data
    ind_data <- round1_df %>% filter(respondent_id == id)
    
    # Scale bandwidth based on confidence (higher confidence = narrower distribution)
    bandwidth <- 0.5 * (1 - ind_data$confidence) + 0.1
    
    # Create density data
    x_values <- seq(0, 5, length.out = 1000)
    density_values <- dnorm(x_values, mean = ind_data$agreement, sd = bandwidth)
    
    # Add to the all_densities data frame
    temp_df <- data.frame(
      x = x_values,
      y = density_values,
      respondent_id = id
    )
    all_densities <- rbind(all_densities, temp_df)
    
    # Create individual plot
    ind_plot <- ggplot() +
      # Add all individual distributions as faint lines
      geom_line(data = group_mean,  
                aes(x = x, y = y, group = respondent_id), 
                color = "grey", alpha = 0.3) +
      # Add this individual's distribution as a highlighted line
      geom_line(data = temp_df, aes(x = x, y = y), 
                color = "blue", linewidth = 1) + 
      geom_vline(xintercept = round1_df$agreement, color = "grey") +
      geom_vline(xintercept = ind_data$agreement, color = "red") +
      labs(title = paste("Respondent", id),
           x = "Agreement Rating",
           y = "Density") +
      theme_minimal() +
      xlim(0, 5)
    
    plots_list[[id]] <- ind_plot
  }
  
  # Also modify the group density plot to include individual distributions
  group_plot <- ggplot() +
    # Add all individual distributions
    geom_line(data = all_densities, 
              aes(x = x, y = y, group = respondent_id, color = factor(respondent_id)),
              alpha = 0.6) +
    # Add the group density
    geom_density(data = round1_df, aes(x = agreement, weight = confidence),
                 fill = "lightblue", alpha = 0.3, color = "darkblue") +
    labs(title = "Group and Individual Distributions",
         x = "Agreement Rating (0-5)",
         y = "Density",
         color = "Respondent ID") +
    theme_minimal() +
    theme(legend.position = "right")
  
  return(c(list(group_plot), plots_list))
}

all <- individual_density_plots()
grid.arrange(grobs = all, ncol = 3)
# running the code above not as a function, but only for one id

    ind_data <- round1_df %>% filter(respondent_id == 2)
    
    # Scale bandwidth based on confidence (higher confidence = narrower distribution)
    bandwidth <- 0.5 * (1 - ind_data$confidence) + 0.1
    
    # Create density data
    x_values <- seq(0, 5, length.out = 100)
    density_values <- dnorm(x_values, mean = ind_data$agreement, sd = bandwidth)
    
    # Add to the all_densities data frame
    temp_df <- data.frame(
      x = x_values,
      y = density_values,
      respondent_id = 1
    )
    all_densities <- rbind(all_densities, temp_df)
    
    # Create individual plot
    ind_plot <- ggplot() +
      # Add all individual distributions as faint lines
      geom_line(data = all_densities %>% filter(respondent_id != id), 
                aes(x = x, y = y, group = respondent_id), 
                color = "grey", alpha = 0.3) +
      # Add this individual's distribution as a highlighted line
      geom_line(data = temp_df, aes(x = x, y = y), 
                color = "blue", linewidth = 1) + 
      geom_vline(xintercept = ind_data$agreement, color = "red") +
      labs(title = paste("Respondent", id),
           x = "Agreement Rating",
           y = "Density") +
      theme_minimal() +
      xlim(0, 5)

ind_plot

