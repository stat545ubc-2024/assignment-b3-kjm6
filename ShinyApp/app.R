library(shiny)
library(tidyverse)

# Load in data NEED TO SWITCH THIS
dataNCHS <- read.csv("C:/Users/katie/Downloads/NCHS_-_Drug_Poisoning_Mortality_by_State__United_States_20241120 (1).csv")

# Define UI for application that draws a graph
ui <- fluidPage(
  titlePanel("US Overdose Mortality 1999 - 2022"), 
  sidebarLayout(
    sidebarPanel(
      selectInput("stateInput", "State",
                  choices = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                              "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                              "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                              "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
                              "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                              "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                              "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                              "West Virginia", "Wisconsin", "Wyoming", "United States")
      ),
      selectInput("sexInput", "Sex",
                  choices = c("Both Sexes", "Male", "Female")
      ),
      selectInput("ageInput", "Age",
                  choices = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+", "All Ages")
      ),
      selectInput("raceInput", "Race",
                  choices = c("All Races - All Origins", "Hispanic", "Non-Hispanic Black", "Non-Hispanic White")
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a graph
server <- function(input, output) {
  output$plot <- renderPlot({
    # Filter the dataset based on user inputs
    filtered <- dataNCHS %>%
      filter(State == input$stateInput,
             Race.and.Hispanic.Origin == input$raceInput,
             Sex == input$sexInput,
             Age.Group == input$ageInput)
    
    # Plot the data
    ggplot(filtered, aes(x = Year, y = Deaths)) +
      geom_line() +
      labs(title = paste("Opioid Deaths in", input$stateInput),
           x = "Year", y = "Deaths") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
