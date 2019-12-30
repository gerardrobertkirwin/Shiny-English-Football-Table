#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Upload packages ---------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggrepel)
library(ggplot2)
library(tibble)
source('table cleaning.R', local=TRUE)


# Define UI ----
ui <- fluidPage(
    
    titlePanel("English Teams Since WWII"),
    sidebarLayout(
        sidebarPanel(
          selectInput("Season", "Choose Season", choices = unique(table$Season)),
          br(),
          uiOutput("LeagueChoices"),
          br()
           ),
        mainPanel(
            
            tableOutput("table1"),
            tags$div(
              tags$p("Note:"),
              tags$p("1. Between 1946-47 and 1957-58 there are only three divisions and the third division is split between Third Division North and Third Division South,
              the two divisions are listed together here together.")
              
            )
        )
    )
)

# Define server logic ----
server <- function(input, output) {
    

    output$table1 <- renderTable(
      
      
      table %>% filter(Season == input$Season, Tier %in% input$Tier) %>% 
                                   select(Tier, Pos, Club, P, W, D, L, F, A, GD, Pts) %>% 
      print()
      
    )
    
    output$LeagueChoices <- renderUI({
      table_choices <- table %>% filter(Season == input$Season)
      checkboxGroupInput("Tier", "Choose League", choices = unique(table_choices$Tier), selected = "First Division")
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)