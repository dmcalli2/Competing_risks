# This code simulates competing risks, following the approach suggested in 
# Competing Risks and Multistate Models with R
# Authors: Beyersmann, Jan, Allignol, Arthur, Schumacher, Martin
# https://www.springer.com/gb/book/9781461420347
# See chapter 3 of that title for additional details

library(tidyverse)
library(ggplot2)

# Function to create dataframe with samples for arm, Welton version
choose_times <- seq(0, 10, 0.5)
MakeCumulative <- function (arm, rate.main, rate.compete, times = choose_times){
  lamda_new <- matrix(NA, ncol = 2, nrow = length(times))
  p_new <- matrix(NA, ncol = 3, nrow = length(times))
  cumslam_new <- NA

  lamda_new [,1] <- rate.main 
  lamda_new [,2] <- rate.compete
  
  slam_new <- rowSums(lamda_new[,1:2]) 
  cumslam_new <- 1 - exp(-slam_new*times)

  # Calculate cumulative incidence for each outcome
  for (j in 1:2){
    p_new[,j] <- lamda_new[,j] * cumslam_new /slam_new 
  }# end of loop through outcomes
    p_new[,3] <- 1 - rowSums(p_new[,1:2])
   p_new
}

## Create set of scenario options.
choose_ci <- expand.grid(rate.main = c(0, 1, seq(10,90, 10))/100,
                         rate.compete = c(0, 1, seq(10,90, 10))/100,
                         tx.main = exp(seq(-0.5, 0.5, 0.25)),
                         tx.compete = exp(seq(-0.5, 0.5, 0.25)))

## Make the no effects scenario the first in the list
first <- choose_ci %>% filter(rate.main == 0.1, rate.compete == 0.01,
                              tx.main == min(tx.main), 
                              tx.compete == min(tx.compete))
choose_ci <- bind_rows(first,
                       choose_ci) %>% 
  distinct() %>% 
  mutate(scenario = seq_along(choose_ci$rate.compete))

## Run the simulation in all the scenarios
simul_res <- map(choose_ci$scenario, function(i){

  # Placebo arm
  pl <- MakeCumulative (arm = "pl",
                                  rate.main = choose_ci$rate.main[i],
                                  rate.compete = choose_ci$rate.compete[i])
  # Treatment arm
  tx <- MakeCumulative (arm = "tx",
                                  rate.main = choose_ci$tx.main[i]*choose_ci$rate.main[i],
                                  rate.compete = choose_ci$tx.compete[i]*choose_ci$rate.compete[i])
  
  ## Calculate comparisons
    arr = pl - tx
    rr = tx/pl
    or = (tx/(1-tx)) / (pl/(1-pl)) 
  
    ## Arrange data for plots
    arms <- bind_rows(tibble(obs.times = choose_times, arm = "pl", n1_cum_per = pl[,1]),
                      tibble(obs.times = choose_times, arm = "tx", n1_cum_per = tx[,1]))
    
    effects <- tibble(obs.times = choose_times,
                      arr = arr[,1], rr = rr[,1], or = or[,1]) %>% 
      filter(obs.times !=0)
  list(arms = arms, effects = effects)
})
simul_res <- transpose(simul_res)

## Save as R objects for inclusion in the Shiny App
saveRDS(bind_rows(simul_res$effects, .id = "scenario"), "simulate_effects.Rds")
saveRDS(bind_rows(simul_res$arms, .id = "scenario"), "simulate_arms.Rds")
saveRDS(choose_ci, "Scenario descriptions.Rds")

## Test the code using a single scenario
effects <- readRDS("simulate_effects.Rds")
arms <- readRDS("simulate_arms.Rds")
scenarios = readRDS("Scenario descriptions.Rds")

## For a single scenario, create plots as per the Shiny app
plot1 <- ggplot (arms %>% filter(scenario %in% 1), aes(x = obs.times, y = n1_cum_per, colour = arm)) + 
  geom_smooth(se = FALSE) +
  scale_x_continuous("Time (years)") +
  scale_y_continuous ("Cumulative incidence") 
plot1

plot2 <- ggplot (effects %>% filter(scenario %in% 1), aes(x = obs.times, y = rr)) + 
  geom_smooth(se = FALSE) +
  scale_x_continuous("Time (years)") +
  scale_y_continuous ("Relative risk for events to time (t)") 
plot2

plot3 <- ggplot (effects %>% filter(scenario %in% 1), aes(x = obs.times, y = or)) + 
  geom_smooth(se = FALSE) +
  scale_x_continuous("Time (years)") +
  scale_y_continuous ("Odds ratio for events to time (t)") 
plot3
