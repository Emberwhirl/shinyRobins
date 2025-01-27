# Updated 2025-01-27
# ROBINS-I V2 latest update on 2024-11-22

# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(dplyr)
library(surveydown)


# Database setup

# surveydown stores data on a database that you define at https://supabase.com/
# To connect to a database, update the sd_database() function with details
# from your supabase database. For this demo, we set ignore = TRUE, which will
# ignore the settings and won't attempt to connect to the database. This is
# helpful for local testing if you don't want to record testing data in the
# database table. See the documentation for details:
# https://surveydown.org/store-data

# The parameters of sd_database should be updated before deployment
db <- sd_database(
  host   = "",
  dbname = "",
  port   = "",
  user   = "",
  table  = "",
  ignore = TRUE  
)


# Server setup
server <- function(input, output, session) {

  # Define the condition for D5_Q11
  # condition_d5_q11 <- function(input){
  #   bool1 = input$D5_Q01 %in% c("PN", "N", "NI") || input$D5_Q02 %in% c("PN", "N", "NI") || input$D5_Q03 %in% c("PN", "N", "NI")
  #   bool2 = (input$D5_Q05 %in% c("Y", "PY", "NI") || (input$D5_Q08 %in% c("Y", "PY") && input$D5_Q09 %in% c("WN", "SN", "NI")) || input$D5_Q10 %in% c("WN", "SN", "NI"))
  #   haha = bool1 && bool2
  #   return(haha)
  # }
  
  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if(
    input$B2 == "Y" || input$B2 == "PY" ~ "end_critical",
    input$B3 == "Y" || input$B3 == "PY" ~ "end_critical"
    )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(
    input$B1 %in% c("PN", "N") ~ "B2",
    input$C2 == "Y" ~ "C3",
    input$E1 == "PP" ~ "E2",
    
    input$C2 == "N" || input$C3 == "N"~ "D1a_Q1",
    input$D1a_Q1 %in% c("Y", "PY", "WN") ~ "D1a_Q2",
    input$D1a_Q1 %in% c("Y", "PY", "WN") ~ "D1a_Q3",
    sd_is_answered("D1a_Q1") ~ "D1a_Q4",
    input$C2 == "Y" && input$C3 == "Y" ~ "D1b_Q1",
    input$D1b_Q1 %in% c("Y", "PY") ~ "D1b_Q2",
    input$D1b_Q2 %in% c("Y", "PY", "WN") ~ "D1b_Q3",
    input$D1b_Q1 %in% c("N", "PN", "NI") ~ "D1b_Q4",  
    sd_is_answered("D1b_Q1") ~ "D1b_Q5",
    sd_is_answered("D1a_Q1") ~ "D1a_overall",
    sd_is_answered("D1a_Q1") ~ "D1a_overall_dir",
    sd_is_answered("D1b_Q1") ~ "D1b_overall",
    sd_is_answered("D1b_Q1") ~ "D1b_overall_dir",
    
    input$D2_Q1 %in% c("Y", "PY", "NI") ~ "D2_Q2",  ## This is different from the table, but same as the flow diagram for algorithm.
    input$D2_Q1 %in% c("N", "PN") ~ "D2_Q3",
    input$D2_Q1 %in% c("N", "PN") && input$D2_Q4 %in% c("WY", "N", "PN", "NI") ~ "D2_Q5",
    
    input$D3_Q01 %in% c("Y", "PY") ~ "D3_Q02",
    input$D3_Q03 %in% c("N", "PN") ~ "D3_Q04",
    input$D3_Q05 %in% c("Y", "PY") ~ "D3_Q06",
    input$D3_Q06 %in% c("Y", "PY") ~ "D3_Q07",
    input$D3_Q02 %in% c("Y", "PY") || input$D3_Q04 %in% c("N", "PN") || input$D3_Q07 %in% c("Y", "PY") ~ "D3_Q08",
    input$D3_Q08 %in% c("N", "PN") ~ "D3_Q09",
    input$D3_Q09 %in% c("N", "PN", "NI") ~ "D3_Q10", ## This is different from the table, but same as flow diagram for algorithm.
    
    input$E1 == "ITT" ~ "D4a_Q1",
    input$D4a_Q1 %in% c("Y", "PY") ~ "D4a_Q2",
    input$D4a_Q1 %in% c("Y", "PY") ~ "D4a_Q3",
    input$D4a_Q2 %in% c("Y", "PY", "NI") || input$D4a_Q3 %in% c("Y", "PY", "NI") ~ "D4a_Q4",
    input$E1 == "ITT" ~ "D4a_Q5",
    sd_is_answered("D4a_Q1") ~ "D4a_overall",
    sd_is_answered("D4a_Q1") ~ "D4a_overall_dir",

    
    input$E1 == "PP"  ~ "D4b_Q1",
    input$D4b_Q1 %in% c("N", "PN", "NI") ~ "D4b_Q2",
    input$D4b_Q2 %in% c("Y", "PY") ~ "D4b_Q3",
    sd_is_answered("D4b_Q1") ~ "D4b_overall",
    sd_is_answered("D4b_Q1") ~ "D4b_overall_dir",
    
    
    (input$D5_Q01 %in% c("N", "PN", "NI") || input$D5_Q02 %in% c("N", "PN", "NI") || input$D5_Q03 %in% c("N", "PN", "NI")) ~ "D5_Q04",
    input$D5_Q04 %in% c("Y", "PY", "NI") ~ "D5_Q05",
    input$D5_Q05 %in% c("Y", "PY", "NI") ~ "D5_Q06",
    input$D5_Q04 %in% c("N", "PN") ~ "D5_Q07",
    input$D5_Q07 %in% c("Y", "PY") ~ "D5_Q08",
    input$D5_Q08 %in% c("Y", "PY") ~ "D5_Q09",
    input$D5_Q07 %in% c("N", "PN", "NI") ~ "D5_Q10",
    # condition_d5_q11(input) ~ "D5_Q11",
    ((input$D5_Q01 %in% c("PN", "N", "NI") || input$D5_Q02 %in% c("PN", "N", "NI") || input$D5_Q03 %in% c("PN", "N", "NI")) && (input$D5_Q05 %in% c("Y", "PY", "NI") || (input$D5_Q08 %in% c("Y", "PY") && input$D5_Q09 %in% c("WN", "SN", "NI")) || input$D5_Q10 %in% c("WN", "SN", "NI"))) ~ "D5_Q11",
    
    input$D6_Q2 %in% c("Y", "PY", "NI") ~ "D6_Q3"
    
  )

  # Database designation and other settings
  sd_server(
    # cookies = TRUE,  ## Strangely, setting 'cookies = FALSE' will crash the application. 
    db = db,
    all_questions_required = TRUE,
    rate_survey = T)

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
