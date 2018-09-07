# Simulate competing risks,. based on book chapter 3
# Cure, Local
# CV 5.5  9.2
# NON-cv 0.7  5.6
library(tidyverse)
library(ggplot2)
library(plyr)
library(scales)

set.seed (2576)
mysamples <- 10000
# Function to create dataframe with samples for arm
MakeCumulative <- function (arm, rate.main, rate.compete, cens.time = 10, samples = mysamples ){
  event.times <- rexp(samples, rate.main + rate.compete)
  f.cause <- rbinom(samples, size = 1, prob = rate.main/(rate.main+rate.compete))
  f.cause <- ifelse(f.cause == 0, 2, 1)
  obs.times <- pmin (event.times,cens.time) # censors at censoring time
  obs.cause <- c(event.times <= cens.time) * f.cause  
  mydf <- data.frame (obs.cause, obs.times)
  mydf$arm <- arm
  mydf[ , c("arm", "obs.times", "obs.cause")]
}

choose_ci <- expand.grid(rate.main = c(10, 20, 40, 80)/100,
                         rate.compete = c(1, 10, 20, 40, 80)/100,
                         tx.main = exp(seq(-0.5, 0.5, 0.25)),
                         tx.compete = exp(seq(-0.5, 0.5, 0.25)))
choose_ci <- choose_ci %>% 
  mutate(scenario = seq_along(choose_ci$rate.compete))

# seq_along(choose_ci$rate.main)
simul_res <- map(choose_ci$scenario, function(i){

  # Run function from each arm
  mydf.cure.pl <- MakeCumulative (arm = "pl",
                                  rate.main = choose_ci$rate.main[i],
                                  rate.compete = choose_ci$rate.compete[i])
  mydf.cure.tx <- MakeCumulative (arm = "tx",
                                  rate.main = choose_ci$tx.main[i]*choose_ci$rate.main[i],
                                  rate.compete = choose_ci$tx.compete[i]*choose_ci$rate.compete[i])
  
  
  AggFunction <- function(mydf){
    mydf %>% 
      mutate(obs.times = round(obs.times, 1)) %>% 
      group_by(arm, obs.times, obs.cause) %>% 
      count() %>% 
      ungroup()  %>% 
      arrange(obs.times) %>% 
      group_by(arm) %>% 
      mutate(n_tot = sum(freq),
             n1 = freq * (obs.cause==1),
             n1_cum = cumsum(n1),
             n1_cum_per = n1_cum/max(n_tot)) %>% 
      ungroup() 
  }

  mydf.cure <- AggFunction(bind_rows(mydf.cure.pl, mydf.cure.tx)) 
  
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
saveRDS(bind_rows(simul_res$effects, .id = "scenario"), "simulate_effects.Rds")
saveRDS(bind_rows(simul_res$arms, .id = "scenario"), "simulate_arms.Rds")
saveRDS(choose_ci, "Scenario descriptions.Rds")

effects <- readRDS("simulate_effects.Rds")
arms <- readRDS("simulate_arms.Rds")
scenarios = readRDS("Scenario descriptions.Rds")


##
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
