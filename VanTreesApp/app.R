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
  plotOutput("id_histogram"),
  tableOutput("id_table")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$id_histogram <- renderPlot({
        ggplot(deathSeriesAnnual, aes(x = year, y = deaths)) +
        geom_point()
    })
    output$id_table <- renderTable({
      deathSeriesAnnual
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
