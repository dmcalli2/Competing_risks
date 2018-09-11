library(shiny)
library(tidyverse)

## Make choices from scenarios
scenarios <- readRDS("Scenario descriptions.Rds")

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

choices <- map(scenarios %>% select(-scenario), ~
                         unique(.x))

names(choices$rate.main) <- 100* choices$rate.main
names(choices$rate.compete) <- 100* choices$rate.compete
names(choices$tx.main) <- round(choices$tx.main, 2)
names(choices$tx.compete) <- round(choices$tx.compete, 2)

# Define UI for application that allows exploration of competing risks
shinyUI(fluidPage(tabsetPanel(
  tabPanel("App",
  # Application title
  titlePanel("Exploring, rates, risks and effect estimates"),
  
  # Sidebar For competing risks
  sidebarLayout(
    sidebarPanel(p(("Choose from 500 different combinations to explore impacts.")),
       radioButtons("rate_main",
                   "Rate of target event per 100 person-years:",
                   choices = choices$rate.main),
       radioButtons("rate_compete",
                   "Rate of competing event per 100 person-years:",
                   choices = choices$rate.compete),
       radioButtons("tx_main",
                   "Rate ratio for effect of treatment on target event:",
                   choices = choices$tx.main),
       radioButtons("tx_compete",
                   "Rate ratio for effect of treatment on competing event:",
                   choices = choices$tx.compete),
       actionButton("reset", "Remove all previous choices from plots")
    ) , # close sidebar
    
    # Show a plot of rates in each arm, and the treatment comparisons
    mainPanel(
      fluidRow(column(plotOutput("plot_arms"), width = 6), column(plotOutput("plot_arr"), width = 6)),
      fluidRow(column(plotOutput("plot_or"), width = 6), column(plotOutput("plot_rr"), width = 6))
      )
    )# close sidebar layout
  ), # Close Panel
  tabPanel("Explanation",
           h1("Description of the app"),
           p("This app is designed to allow users to explore the effect of different rates of target and competing events on risks.
             The intention is for users to be able to get a feel for the impact of these, without the distraction of having
             to analyse data or write code. There are 500 different combinations of rates and effects to explore."),
           h1("Getting started"),
           p("Leaving all the other settings as they are, work through the 'Rate of target event' options.
              This shows the effect of increasing incidence rates on the cumulative incidence (risk).
              Next, go back to the lowest rate and click 'Remove all previous choices from plots', then work through the 'Rate of competing event' options.
              When you are comfortable with rates of target and competing events, you can add in treatment effects."),
           h1("Defintions"),
           p("Target event - the event of interest (eg death from cardiovascular disease in a cardiovascular trial)"),
           p("Competing event - some other event which prevents someone from experiencing the target event (eg death from any other cause)"),
           p("Rate - instantaneous rate, events per person-time"),
           p("Risk - number of events as a proportion of the number of people at-risk at baseline"),
           p("Odds ratio - odds of experiencing event among people in the treatment group compared to control group"),
           p("Risk ratio - risk of experiencing event among people in the treatment group compared to the control group"),
           p("Absolute risk reduction - absolute reduction in risk in people in the treatment group comapred to the control group"),
           h1("References"),
           p("For a discussion of competing risks and a description of the simulations run to create these scenarios see chapter 3 of 'Competing Risks and Multistate Models with R'
             by Beyersmann, Jan, Allignol, Arthur, Schumacher, Martin, available at:-"),
           a(href = "https://www.springer.com/gb/book/9781461420347", "www.springer.com/gb/book/9781461420347"),
           p("for the code used to run the simulation and create the app, see:-"),
           a(href = "https://github.com/dmcalli2/Competing_risks.git", "https://github.com/dmcalli2/Competing_risks.git"))
  )
  )#Close TabSetPanel
  )# CLose ShinyUI

