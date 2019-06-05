#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
source("final_work.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Iowa Temperature and West Nile (2007-2017)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # select X variable
         radioButtons("xvar", "X Variable", 
                      choices = c("Year", "Average Temperature", "West Nile Cases"),
                      selected = "Average Temperature"),
        # Select Y variable 
         radioButtons("yvar", "Y Variable", 
                      choices = c("Year", "Average Temperature", "West Nile Cases"),
                      selected = "West Nile Cases"),
         # choose type of winter
         radioButtons("winter_type", "Winter Metric", 
                      choices = c("Traditional", "Meteorlogical")),
         # help note
         helpText("Note: traditional winter refers to December 1st - March 1st,
                  while meteorlogical winter refers to December 15th - March 15th"),
         # conditional choice between dry and wet
         conditionalPanel( condition = "input.xvar == 'Average Temperature'",
           checkboxInput("temp_type", 
                       "Change from Average Dry Temperature to Average Wet Temperature",
                       value = FALSE)),
         conditionalPanel( condition = "input.yvar == 'Average Temperature'",
                           checkboxInput("temp_type", 
                                         "Change from Average Dry Temperature to Average Wet Temperature",
                                         value = FALSE))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         h6("Note: For winter 2012 there is missing data for hourly bulb temperature.")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #switch xvar to string for aes_string()
  xvar_choice <- reactive ({case_when(
                          input$xvar == "Year" ~ "year",
                          input$xvar == "Average Temperature" & input$temp_type == TRUE ~ "avg_wet_year",
                          input$xvar == "Average Temperature" & input$temp_type == FALSE ~ "avg_dry_year",
                          input$xvar == "West Nile Cases" ~ "westnile")})
  
# Titles for X axis
    xvar_title <-reactive ({ switch(input$xvar,
                          "Year" = "Year",
                          "Average Temperature" = "Average Temperature in Degrees",
                          "West Nile Cases" = "Number of West Nile Cases per Year")})
    #switch yvar to string for aes_string()  
   yvar_choice <- reactive ({case_when(
     input$yvar == "Year" ~ "year",
     input$yvar == "Average Temperature" & input$temp_type == TRUE ~ "avg_wet_year",
     input$yvar == "Average Temperature" & input$temp_type == FALSE ~ "avg_dry_year",
     input$yvar == "West Nile Cases" ~ "westnile")})
   
  #Titles for Y var  
    yvar_title <- reactive ({ switch(input$yvar,
                                     "Year" = "Year",
                                     "Average Temperature" = "Average Temperature in Degrees",
                                     "West Nile Cases" = "Number of West Nile Cases per Year")})
  # choose dataset based on winter 
    winter_choice <- reactive ({switch(input$winter_type,
                            "Traditional" = iowa_t_west_nile,
                            "Meteorlogical" = iowa_m_west_nile)})
    
  
   
  
   
   output$distPlot <- renderPlot({
     
     
     if(input$xvar != "Year" & input$yvar != "Year"){
       ggplot(winter_choice(), aes_string(xvar_choice(), yvar_choice())) +
         geom_line()+
         geom_point(aes(color = year), size = 4)+
         #geom_smooth(method = "lm", fill = NA)+
         labs(x = xvar_title(), y = yvar_title(), color = "Year")+
         theme_minimal()+
         theme(axis.title = element_text(size = 14))
       
     }else{
       ggplot(winter_choice(), aes_string(xvar_choice(), yvar_choice())) +
         geom_line()+
         geom_point(size = 4)+
        # geom_smooth(method = "lm", fill = NA)+
         labs(x = xvar_title(), y = yvar_title())+
         theme_minimal()+
         theme(axis.title = element_text(size = 14))
       
     }

      
       
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

