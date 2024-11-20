#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(datateachr)
library(tidyverse)

deathSeriesAnnual <- read.delim("C:/Users/katie/Downloads/DeathSeriesAnnual.tab")

# Define UI for application that draws a histogram
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
                              "Division 9: Pacific"
                  )
      ),
      sliderInput("yearInput", "Year", min = 1999, max = 2021, value = c(1999, 2021))
    ),
    mainPanel(
      plotOutput("id_point"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$id_point <- renderPlot({
        ggplot(deathSeriesAnnual, aes(x = age, y = deaths)) +
        geom_point()
    })
    output$id_table <- renderTable({
      deathSeriesAnnual
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
