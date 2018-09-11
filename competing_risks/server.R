library(shiny)
library(tidyverse)
library(plotly)

# effects <- readRDS("simulate_effects.Rds")
# arms <- readRDS("simulate_arms.Rds")
# scenarios <- readRDS("Scenario descriptions.Rds")

## Make cumulative incidence plots
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


## Relabel arms to make them more informative
arms <-arms %>% 
  mutate(arm = if_else(arm == "tx", "Treatment", "Control"),
         risk = n1_cum_per *100) %>% 
  rename(year = obs.times)

## Make text labels based on scenario choices
scenarios <- scenarios %>% 
  mutate(output_text = paste0("Rate main = ", rate.main,
                              ", Rate compete = ", rate.compete,
                              ", RR main = ", tx.main %>% round(2),
                              " and RR compete = ", tx.compete %>% round(2)))
first_scenario <- scenarios %>% 
  filter(rate.main == min(rate.main), rate.compete == min(rate.compete),
         tx.main == 1, tx.compete == 1) 

scenarios <- bind_rows(first_scenario,
                       scenarios) %>% 
  distinct()
  
first_scenario <- first_scenario  %>% 
  pull(scenario)
  
# Create plots for selected scenarios
shinyServer(function(input, output) {
  
  ## For initial value on opening app
  scenarios_combine <- reactiveValues(one = c(first_scenario))


  scenario_choose <- reactive({
        # Select data based on choices
    scenarios <- scenarios %>% 
      filter((rate.main == input$rate_main &
             rate.compete == input$rate_compete &
             tx.main == input$tx_main &
             tx.compete == input$tx_compete)) %>% 
      slice(1)
    scenarios$scenario
    # 
  })
  ## Combine last value with newest, and take two most recent
  observe({
        scenarios_combine$one <- c(isolate(scenarios_combine$one), scenario_choose()) 
  
      })
    ## If reset button pressed
         observeEvent(input$reset, scenarios_combine$one <- scenarios_combine$one %>% tail(1))
         
  ## Create text to describe each scenario
  scenario_descriptions <- reactive({
    scenario_for_text <- tibble(scenario = scenarios_combine$one)
    scenario_for_text <- scenario_for_text %>%
      left_join(scenarios %>% select(scenario, output_text)) 
    
    paste0(c("Greyed lines: ", "Dark lines: "), scenario_for_text$output_text) 
})
  output$scenario1 <- renderText(scenario_descriptions() %>% tail(2) %>%  head(1))
  output$scenario2 <- renderText(scenario_descriptions() %>% tail(1))


  output$plot_arms <- renderPlot({
     arms <- arms %>% 
      filter(scenario %in% scenarios_combine$one) %>% 
       mutate(scenario_old = scenario == tail(scenarios_combine$one, 1))

  ggplot (arms, aes(x = year, y = risk, colour = arm, alpha = scenario_old,  
                      group = interaction(arm,scenario))) + 
      geom_step() +
      scale_x_continuous("Time (years)", breaks = seq(2,10, 2), minor_breaks = 1:20, limits = c(0, 10)) +
      scale_y_continuous ("Cumulative incidence (risk, %)", limits = c(0,100), breaks = seq(20, 100, 20),
                          minor_breaks = seq(10, 100, 10)) +
      scale_alpha_discrete(range = c(0.25, 1), guide = FALSE) 

  })
  
    output$plot_rr <- renderPlot({
     effects <- effects %>% 
      filter(scenario %in% scenarios_combine$one) %>% 
       mutate(scenario_old = scenario == tail(scenarios_combine$one, 1))

    ggplot (effects, aes(x = obs.times, y = log(rr), alpha = scenario_old, group = scenario)) + 
      geom_step() +
      scale_x_continuous("Time (years)", breaks = seq(1,10, 1), limits = c(0, 10)) +
      scale_y_continuous ("Relative risk for events to time (t)", breaks = seq(-0.5, 0.5, 0.25),
                     labels = round(exp(seq(-0.5, 0.5, 0.25)),2),
                     limits = c(-0.75,0.75)) +
      scale_alpha_discrete(range = c(0.5, 1), guide = FALSE)
    
  })
  
    output$plot_or <- renderPlot({
     effects <- effects %>% 
      filter(scenario %in% scenarios_combine$one) %>% 
       mutate(scenario_old = scenario == tail(scenarios_combine$one, 1))

    ggplot (effects, aes(x = obs.times, y = log(or), alpha = scenario_old, group = scenario)) + 
      geom_step() +
      scale_x_continuous("Time (years)", breaks = seq(1,10, 1), limits = c(0, 10)) +
      scale_y_continuous ("Odds for events to time (t)", breaks = seq(-0.5, 0.5, 0.25),
                     labels = round(exp(seq(-0.5, 0.5, 0.25)),2),
                     limits = c(-0.75,0.75)) +
      scale_alpha_discrete(range = c(0.5, 1), guide = FALSE)
    
  })  
    output$plot_arr <- renderPlot({
     effects <- effects %>% 
      filter(scenario %in% scenarios_combine$one) %>% 
       mutate(scenario_old = scenario == tail(scenarios_combine$one, 1))

   ggplot (effects, aes(x = obs.times, y = arr, alpha = scenario_old,  group = scenario)) + 
      geom_step() +
      scale_x_continuous("Time (years)", breaks = seq(1,10, 1), limits = c(0, 10)) +
      scale_y_continuous ("ARR for events to time (t)") +
      scale_alpha_discrete(range = c(0.5, 1), guide = FALSE)
    
  })
})
