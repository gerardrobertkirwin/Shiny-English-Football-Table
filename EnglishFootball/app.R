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
library(DT)
source('table cleaning.R', local=TRUE)


# Define UI ----
ui <- navbarPage(
    title = "English Teams Since WWII",
        tabPanel('Table',
        
#Table-----    
            sidebarLayout(
                sidebarPanel(
                    selectInput("Season", "Choose Season", choices = unique(table$Season)),
                    br(),
                    uiOutput("LeagueChoices"),
                    br(),
                    helpText("1. Between 1946-47 and 1957-58 there are only three divisions and the third division is split between Third Division North and Third Division South,
                      the two divisions are listed together here together.")
                ),
                mainPanel(
                    
                    dataTableOutput("table1")
                   
                    ) 
                )
        ),
#Line graph----
        tabPanel('Year by Year',
            mainPanel(
                plotlyOutput('line1',  height = "650px", width = "150%")     
                 
                 
        ))
#,
#Bar graphs----        
        #tabPanel('Leaders',
        #    sidebarLayout(
        #     sidebarPanel(
          #       radioButtons("type", "Select ranking by:",
            #                  c("Wins" = "`Total Wins`",
            #                    "Losses" = "`Total Draws`",
            #                    "Points" = "`Total Points`",
            #                    "Goal Difference" = "`Total Goal Differential`")),
            #     
             #                    br(),
             #    
             #    sliderInput("n",
             #                "Select Number of Teams:",
             #                value = 10,
             #                min = 1,
             #                max = 121)
             #    
             #),
             
             
             #mainPanel(  
             #    tabsetPanel(type = "tabs",
             #                tabPanel("Plot", plotOutput("bar1")),
             #                tabPanel("Table", tableOutput("table2"))
             #   )
             #)
             #)
        #)
)

# Define server logic ----
server <- function(input, output) {
    
#The table from part 1-----     
    output$table1 <- renderDataTable(
        
        
        table %>% filter(Season == input$Season, Tier %in% input$Tier) %>% 
            select(Tier, Pos, Club, P, W, D, L, F, A, GD, Pts) %>% 
            print(),
        
        options = list(paging=FALSE, scrollX = TRUE, searching=FALSE), rownames=FALSE)
    
    output$LeagueChoices <- renderUI({
        table_choices <- table %>% filter(Season == input$Season)
        checkboxGroupInput("Tier", "Choose Division", choices = unique(table_choices$Tier), selected = "First Division")
    })
    
#The plotly line graph----
    output$line1 <- renderPlotly({
        all_plot <- ggplot(table, aes(x=Season, y=`Total Position`, group=Club, text= paste(Tier, "</br>", paste(W, D, L, sep = "-"), "</br>", "Points:", Pts) )) + 
            geom_line(aes(color=Club), size=.5) + 
            geom_point(aes(color=Club), size=.5) +
            scale_colour_grey() +
            theme_classic() +
            scale_y_reverse(breaks = c(1, 20, 40, 60, 80)) +
            scale_x_discrete(breaks=c("1946-47", "1956-57", "1966-67", "1976-77", "1986-87","1996-97", "2006-07", "2016-17"))
        
        line <- ggplotly(all_plot, tooltip = c("color", "group", "x", "text")) %>%   
            config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "toggleSpikelines", "autoScale2d")) %>% 
            style(visible="legendonly", traces = c(1:5, 7:28, 30:56, 58:60, 62:65, 67:85, 87, 89:114, 116:120))
        print(line)
        })
    
#A bar graph with selectable bins----        
    
    
#    output$bar1 <- renderPlot({
#        bar_plot <- ggplot(table_postwar_total, aes(x=Club, y=input$type)) +
#                    geom_histogram(binwidth = input$n) + 
#                    theme_classic()   
#        print(bar_plot)
#    })
#        
#    output$table2 <- renderDataTable(
#            
#            
#            table_postwar_total %>%  
#                arrange(desc(input$type)) %>% 
#                print(),
#            
#            options = list(paging=FALSE, scrollX = TRUE), rownames=FALSE)
        
        
}

# Run the app ----
shinyApp(ui = ui, server = server)