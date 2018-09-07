# This code simulates competing risks, following the approach suggested in 
# Competing Risks and Multistate Models with R
# Authors: Beyersmann, Jan, Allignol, Arthur, Schumacher, Martin
# https://www.springer.com/gb/book/9781461420347
# See chapter 3 of that title for additional details

library(tidyverse)
library(ggplot2)

set.seed (2576)
mysamples <- 10000
# Function to create dataframe with samples for arm
MakeCumulative <- function (arm, rate.main, rate.compete, cens.time = 10, samples = mysamples ){
  event.times <- rexp(samples, rate.main + rate.compete) # Exponential distribution for time
  f.cause <- rbinom(samples, size = 1, prob = rate.main/(rate.main+rate.compete)) # Bernoulli distribution
  f.cause <- ifelse(f.cause == 0, 2, 1)
  obs.times <- pmin (event.times,cens.time) # censors events occuring after censoring time
  obs.cause <- c(event.times <= cens.time) * f.cause  
  mydf <- data.frame (obs.cause, obs.times)
  mydf$arm <- arm
  mydf[ , c("arm", "obs.times", "obs.cause")]
}

## Create set of scenario options.
choose_ci <- expand.grid(rate.main = c(10, 20, 40, 80)/100,
                         rate.compete = c(1, 10, 20, 40, 80)/100,
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
  mydf.cure.pl <- MakeCumulative (arm = "pl",
                                  rate.main = choose_ci$rate.main[i],
                                  rate.compete = choose_ci$rate.compete[i])
  # Treatment arm
  mydf.cure.tx <- MakeCumulative (arm = "tx",
                                  rate.main = choose_ci$tx.main[i]*choose_ci$rate.main[i],
                                  rate.compete = choose_ci$tx.compete[i]*choose_ci$rate.compete[i])
  
  # Aggregate data my rouding event times, done to reduce size of dataset
  AggFunction <- function(mydf){
    mydf %>% 
      mutate(obs.times = round(obs.times, 1)) %>% 
      group_by(arm, obs.times, obs.cause) %>% 
      count() %>% 
      ungroup()  %>% 
      arrange(obs.times) %>% 
      group_by(arm) %>% 
      mutate(n_tot = sum(n),
             n1 = n * as.integer(obs.cause==1),
             n1_cum = cumsum(n1),
             n1_cum_per = n1_cum/max(n_tot)) %>% 
      ungroup() 
  }

  mydf.cure <- AggFunction(bind_rows(mydf.cure.pl, mydf.cure.tx)) 
  
  ## reshape data to alloc calculation of comparisons
  mydf.cure.pl2 <- mydf.cure %>% 
    filter(arm == "pl") %>% 
    distinct(obs.times, n1_cum_per) 
  mydf.cure.tx2 <- mydf.cure %>% 
    filter(arm == "tx") %>% 
    distinct(obs.times, n1_cum_per) 
  mydf.cure.pl2$n1_cum_per[1] <- NA

mydf.cure2 <- mydf.cure %>%
    distinct(obs.times) %>% 
    left_join(mydf.cure.pl2 %>%  select(obs.times, pl = n1_cum_per)) %>% 
    left_join(mydf.cure.tx2 %>%  select(obs.times, tx = n1_cum_per)) %>% 
    mutate_at(vars(pl, tx), function(x) if_else(is.na(x), lag(x, default = 0L),  x)) %>% 
    mutate(arr = pl - tx,
           rr = tx/pl,
           or = (tx/(1-tx)) / (pl/(1-pl)) ) %>% 
    mutate(rr = if_else(rr %in% c(Inf, -Inf), NA_real_, rr),
           or = if_else(or %in% c(Inf, -Inf), NA_real_, or)) %>% 
    distinct(obs.times, arr, rr, or) 
  
  list(arms = mydf.cure, effects = mydf.cure2)
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
  geom_step() +
  scale_x_continuous("Time (years)") +
  scale_y_continuous ("Cumulative incidence") 
plot1

plot2 <- ggplot (mydf.cure2, aes(x = obs.times, y = rr)) + 
  geom_step() +
  scale_x_continuous("Time (years)") +
  scale_y_continuous ("Relative risk for events to time (t)") 
plot2

plot3 <- ggplot (mydf.cure3, aes(x = obs.times, y = arr)) + 
  geom_step() +
  scale_x_continuous("Time (years)") +
  scale_y_continuous ("ARR for events to time (t)") 
plot3
