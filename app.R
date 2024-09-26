# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
library(dplyr)
library(glue)

# Database setup

# surveydown stores data on a database that you define at https://supabase.com/
# To connect to a database, update the sd_database() function with details
# from your supabase database. For this demo, we set ignore = TRUE, which will
# ignore the settings and won't attempt to connect to the database. This is
# helpful for local testing if you don't want to record testing data in the
# database table. See the documentation for details:
# https://surveydown.org/store-data

# database setup
db <- sd_database(
  host   = "aws-0-eu-west-2.pooler.supabase.com",
  dbname = "postgres",
  port   = "6543",
  user   = "postgres.msujfghwfbdvnnejymgi",
  table  = "AdultBlock1"
)

# Server setup
server <- function(input, output, session) {

  # Using URL parameters to obtain prolific PIDs
  sd_store_value(sd_get_url_pars("PROLIFIC_PID"), id = "PROLIFIC_PID")
  sd_store_value(sd_get_url_pars("SESSION_ID"), id = "SESSION_PID")
  sd_store_value(sd_get_url_pars("STUDY_ID"), id = "STUDY_ID")
    
  
  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if(
    input$screenout == "blue" ~ "end_screenout",
    input$consent == "no" ~ "end_consent",
    input$consent_understand == "no" ~ "end_consent"
  )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(
    input$cbc_practice_best %in% c("tired") ~ "cbc_practice_w_tired",
    input$cbc_practice_best %in% c("walking") ~ "cbc_practice_w_walking",
    input$cbc_practice_best %in% c("sports") ~ "cbc_practice_w_sports",
    input$cbc_practice_best %in% c("concentration") ~ "cbc_practice_w_concentration",
    input$cbc_practice_best %in% c("embarrassed") ~ "cbc_practice_w_embarrassed",
    input$cbc_practice_best %in% c("unhappiness") ~ "cbc_practice_w_unhappiness",
    input$cbc_practice_best %in% c("treated") ~ "cbc_practice_w_treated",
    
    input$cbc_q1 %in% c("tired") ~ "cbc_q1_w_tired",
    input$cbc_q1 %in% c("walking") ~ "cbc_q1_w_walking",
    input$cbc_q1 %in% c("sports") ~ "cbc_q1_w_sports",
    input$cbc_q1 %in% c("concentration") ~ "cbc_q1_w_concentration",
    input$cbc_q1 %in% c("embarrassed") ~ "cbc_q1_w_embarrassed",
    input$cbc_q1 %in% c("unhappiness") ~ "cbc_q1_w_unhappiness",
    input$cbc_q1 %in% c("treated") ~ "cbc_q1_w_treated",
    
    input$cbc_q2 %in% c("tired") ~ "cbc_q2_w_tired",
    input$cbc_q2 %in% c("walking") ~ "cbc_q2_w_walking",
    input$cbc_q2 %in% c("sports") ~ "cbc_q2_w_sports",
    input$cbc_q2 %in% c("concentration") ~ "cbc_q2_w_concentration",
    input$cbc_q2 %in% c("embarrassed") ~ "cbc_q2_w_embarrassed",
    input$cbc_q2 %in% c("unhappiness") ~ "cbc_q2_w_unhappiness",
    input$cbc_q2 %in% c("treated") ~ "cbc_q2_w_treated",

    input$cbc_q3 %in% c("tired") ~ "cbc_q3_w_tired",
    input$cbc_q3 %in% c("walking") ~ "cbc_q3_w_walking",
    input$cbc_q3 %in% c("sports") ~ "cbc_q3_w_sports",
    input$cbc_q3 %in% c("concentration") ~ "cbc_q3_w_concentration",
    input$cbc_q3 %in% c("embarrassed") ~ "cbc_q3_w_embarrassed",
    input$cbc_q3 %in% c("unhappiness") ~ "cbc_q3_w_unhappiness",
    input$cbc_q3 %in% c("treated") ~ "cbc_q3_w_treated",

    input$cbc_q4 %in% c("tired") ~ "cbc_q4_w_tired",
    input$cbc_q4 %in% c("walking") ~ "cbc_q4_w_walking",
    input$cbc_q4 %in% c("sports") ~ "cbc_q4_w_sports",
    input$cbc_q4 %in% c("concentration") ~ "cbc_q4_w_concentration",
    input$cbc_q4 %in% c("embarrassed") ~ "cbc_q4_w_embarrassed",
    input$cbc_q4 %in% c("unhappiness") ~ "cbc_q4_w_unhappiness",
    input$cbc_q4 %in% c("treated") ~ "cbc_q4_w_treated",

    input$cbc_q5 %in% c("tired") ~ "cbc_q5_w_tired",
    input$cbc_q5 %in% c("walking") ~ "cbc_q5_w_walking",
    input$cbc_q5 %in% c("sports") ~ "cbc_q5_w_sports",
    input$cbc_q5 %in% c("concentration") ~ "cbc_q5_w_concentration",
    input$cbc_q5 %in% c("embarrassed") ~ "cbc_q5_w_embarrassed",
    input$cbc_q5 %in% c("unhappiness") ~ "cbc_q5_w_unhappiness",
    input$cbc_q5 %in% c("treated") ~ "cbc_q5_w_treated",

    input$cbc_q6 %in% c("tired") ~ "cbc_q6_w_tired",
    input$cbc_q6 %in% c("walking") ~ "cbc_q6_w_walking",
    input$cbc_q6 %in% c("sports") ~ "cbc_q6_w_sports",
    input$cbc_q6 %in% c("concentration") ~ "cbc_q6_w_concentration",
    input$cbc_q6 %in% c("embarrassed") ~ "cbc_q6_w_embarrassed",
    input$cbc_q6 %in% c("unhappiness") ~ "cbc_q6_w_unhappiness",
    input$cbc_q6 %in% c("treated") ~ "cbc_q6_w_treated",

    input$cbc_q7 %in% c("tired") ~ "cbc_q7_w_tired",
    input$cbc_q7 %in% c("walking") ~ "cbc_q7_w_walking",
    input$cbc_q7 %in% c("sports") ~ "cbc_q7_w_sports",
    input$cbc_q7 %in% c("concentration") ~ "cbc_q7_w_concentration",
    input$cbc_q7 %in% c("embarrassed") ~ "cbc_q7_w_embarrassed",
    input$cbc_q7 %in% c("unhappiness") ~ "cbc_q7_w_unhappiness",
    input$cbc_q7 %in% c("treated") ~ "cbc_q7_w_treated",

    input$cbc_q8 %in% c("tired") ~ "cbc_q8_w_tired",
    input$cbc_q8 %in% c("walking") ~ "cbc_q8_w_walking",
    input$cbc_q8 %in% c("sports") ~ "cbc_q8_w_sports",
    input$cbc_q8 %in% c("concentration") ~ "cbc_q8_w_concentration",
    input$cbc_q8 %in% c("embarrassed") ~ "cbc_q8_w_embarrassed",
    input$cbc_q8 %in% c("unhappiness") ~ "cbc_q8_w_unhappiness",
    input$cbc_q8 %in% c("treated") ~ "cbc_q8_w_treated",

    input$cbc_q9 %in% c("tired") ~ "cbc_q9_w_tired",
    input$cbc_q9 %in% c("walking") ~ "cbc_q9_w_walking",
    input$cbc_q9 %in% c("sports") ~ "cbc_q9_w_sports",
    input$cbc_q9 %in% c("concentration") ~ "cbc_q9_w_concentration",
    input$cbc_q9 %in% c("embarrassed") ~ "cbc_q9_w_embarrassed",
    input$cbc_q9 %in% c("unhappiness") ~ "cbc_q9_w_unhappiness",
    input$cbc_q9 %in% c("treated") ~ "cbc_q9_w_treated",

    input$cbc_q10 %in% c("tired") ~ "cbc_q10_w_tired",
    input$cbc_q10 %in% c("walking") ~ "cbc_q10_w_walking",
    input$cbc_q10 %in% c("sports") ~ "cbc_q10_w_sports",
    input$cbc_q10 %in% c("concentration") ~ "cbc_q10_w_concentration",
    input$cbc_q10 %in% c("embarrassed") ~ "cbc_q10_w_embarrassed",
    input$cbc_q10 %in% c("unhappiness") ~ "cbc_q10_w_unhappiness",
    input$cbc_q10 %in% c("treated") ~ "cbc_q10_w_treated"
  )

  # Database designation and other settings
  sd_server(
    db = db,
    all_questions_required = TRUE,
    admin_page = TRUE
  )

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
