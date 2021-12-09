library(shiny)
library(shinythemes)
library(docstring)
library(tidyverse)
library(ggplot2)
library(lubridate)



population <- function(df){
  #' Returns the loaded data after converting relevant columns to factors
  #' @return the study data frame
  covid <- read.csv("C:\\Users\\ADMIN\\Desktop\\FINAL PROJECT\\us_states_covid19_daily.csv")
  covid <- transform(covid, date = as.Date(as.character(date), "%Y%m%d"))
  
  covid<-covid[-which(covid[,2] %in% c("VI","AS","GU","MP","PR")),]
  #Since data is cumulative we only look at the last day data for the whole month
  df <-covid %>% 
    mutate(month=month(date)) %>%
    group_by(state) %>%
    arrange(date) %>%
    filter(month != lead(month))%>%
    select(c(date, state, positive, negative))
  
  df[is.na(df)]<-0
  return(df)
}

population(df)

# Define UI for application 
ui <- fluidPage( theme = shinytheme("cerulean"),
                 
    # Application title
    titlePanel("COVID-19 POOL TESTING"),
    navbarPage("Introduction",
    tabPanel(icon("home"),
             
    # Adding text
    p("The novel coronavirus pandemic (COVID-19) is a highly contagious respiratory disease COVID-19 resulting from the SARS-CoV-2 virus,
  first originating in the United States in January 2020. Blood Specimens from patients with suspected coronavirus disease 2019 (COVID-19)
  undergo real-time reverse transcriptase-polymerase chain reaction (RT-PCR) testing for qualitative severe acute respiratory syndrome
  coronavirus 2 (SARS-CoV-2) RNA detection. However, the quick spread of the virus, high demand vis-avis shortage capacity of SARS-CoV-2 RT-PCR testing
  reagents has prompted researchers to consider pool testing strategies. Pooled testing of samples presents itself as a valid mechanism 
  to overcome these hurdles and to achieve cost effective rapid, large-scale testing with lower dependence on test reagents",
  style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
  
  br(),

  p(strong("Pool testing")," strategies build on testing a pooled sample from several patients. If the results from the pool test are negative, all patients in the
    pooled sample are declared not to have COVID-19, but if the results of the pool are positive, each patient sample is tested individually.
    The pooled testing strategy is appealing, particularly when test availability is limited. However, any test sensitivity less than 100% bears the risk
    of a false-negative result for the entire pool. To support informed decision-making regarding the implementation of pool testing for COVID-19, we have
    developed a probabilistic model to estimate the risk of false negatives considering 3 determining factors: COVID-19 prevalence, test sensitivity, and 
    patient pool size",style="text-align:justify;color:black;background-color:lightpink;padding:15px;border-radius:10px"),
  
  br(),
  
  p("The data used in this application are publicly available on Kaggle. These datasets are originally adapted from the New York Times and the 
  COVID-19 Tracking Project.The introductory information about COVID-19 and pooled testing simulation is adapted from the JAMA Network.
  The website links are also included below...",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
  
  p("For access to the original data set on",strong("Kaggle"),", click",
    a(href=" https://www.kaggle.com/sudalairajkumar/covid19-in-usa?select=us_states_covid19_daily.csv", "here",target="_blank"),style="text-align:center;color:black"),
  
  
  p("For more information on simulation of Covid-19 pooled testing, please check the",strong("JAMA Network Open"),"by clicking",
    a(href="https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2767513?resultClick=3", "here",target="_blank"),style="text-align:center;color:black"),
  
  hr(),
  p(em("Developed by"),br(strong("Abraham Liu, Rophence Ojiambo and Anusha Kumar")),style="text-align:center; font-family: arial"),
 
  
  
  #show a plot

    )))

server <- function(input, output) {}
shinyApp(ui = ui, server = server) 
  

