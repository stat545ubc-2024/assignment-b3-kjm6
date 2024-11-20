#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

deathSeriesAnnual <- read.delim("C:/Users/katie/Downloads/DeathSeriesAnnual.tab")

# Define UI for application that draws a graph
ui <- fluidPage(
  titlePanel("US Opioid Deaths 1999 - 2021"), 
  sidebarLayout(
    sidebarPanel(
      selectInput("locationInput", "Location",
                  choices = c("Division 1: New England", 
                              "Division 2: Middle Atlantic", 
                              "Division 3: East North Central", 
                              "Division 4: West North Central", 
                              "Division 5: South Atlantic", 
                              "Division 6: East South Central", 
                              "Division 7: West South Central", 
                              "Division 8: Mountain", 
                              "Division 9: Pacific")
                  ),
      selectInput("ageInput", "Age",
                  choices = c("0+","18+", "0-44", "45-64")
                  ),
    ),
    mainPanel(
      plotOutput("plot"),
    )
  )
)

# Define server logic required to draw a graph
server <- function(input, output) {
    output$plot <- renderPlot({
      filtered <- deathSeriesAnnual %>% 
        filter(state == input$locationInput,
               age == input$ageInput,
               ucdtitle == "Drug",
               mcdtitle == "Opioid",
               race != "All",
               gender == "All"
               )
        ggplot(filtered, aes(x = year, y = deaths, color = race)) +
        geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
