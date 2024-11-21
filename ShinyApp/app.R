library(shiny)
library(tidyverse)
library(shinythemes)
library(ggthemes) 
library(shinyjs)
library(colourpicker)
library(DT)  # To render the data table

# Load in data
dataNCHS <- read.csv("C:/Users/katie/Downloads/NCHS_-_Drug_Poisoning_Mortality_by_State__United_States_20241120 (1).csv")
dataNCHS$Deaths <- as.numeric(gsub(",", "", dataNCHS$Deaths)) 
dataNCHS$Year <- as.numeric(dataNCHS$Year) 
dataNCHS <- dataNCHS %>% 
  select(c("State", "Year", "Sex", "Age.Group", "Race.and.Hispanic.Origin", "Deaths"))

# Define UI for application that draws a graph
ui <- fluidPage(theme = shinytheme("superhero"),
                useShinyjs(),  
                titlePanel("US Overdose Mortality 1999 - 2022"), 
                sidebarLayout(
                  sidebarPanel(
                    selectInput("groupBy", "Group by",
                                choices = c("Age Group", "Race", "Sex"), 
                                selected = "Age Group"
                    ),
                    selectInput("agegroupInput", "Age Group",
                                choices = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+", "All Ages"),
                                selected = "All Ages"
                    ),
                    selectInput("sexInput", "Sex",
                                choices = c("Both Sexes", "Male", "Female"),
                                selected = "Both Sexes"
                    ),
                    selectInput("raceInput", "Race",
                                choices = c("All Races-All Origins", "Hispanic", "Non-Hispanic Black", "Non-Hispanic White"),
                                selected = "All Races-All Origins"
                    ),
                    colourInput("lineColor", "Select Line Color for Total Nationwide Mortality", value = "#FF0000")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Demographic Makeup of Mortality", 
                               plotOutput("plot")  # First tab for the plot
                      ),
                      tabPanel("Mortality by State", 
                               DTOutput("data_table")  # Second tab for the data table
                      )
                    )
                  )
                )
)

# Define server logic required to draw a graph
server <- function(input, output, session) {
  # Enable or disable selectors based on grouping selector
  observe({
    if (input$groupBy == "Age Group") {
      enable("sexInput")
      enable("raceInput")
      disable("agegroupInput")
    } else if (input$groupBy == "Race") {
      enable("sexInput")
      enable("agegroupInput")
      disable("raceInput")
    } else if (input$groupBy == "Sex") {
      enable("agegroupInput")
      enable("raceInput")
      disable("sexInput")
    }
  })
  
  output$plot <- renderPlot({
    # Filter data based on grouping input
    if (input$groupBy == "Age Group") {
      filtered <- dataNCHS %>%
        filter(Race.and.Hispanic.Origin == input$raceInput,
               Sex == input$sexInput,
               Age.Group != "All Ages",
               State == "United States")
    } else if (input$groupBy == "Race") {
      filtered <- dataNCHS %>%
        filter(Age.Group == input$agegroupInput,
               Sex == input$sexInput,
               Race.and.Hispanic.Origin != "All Races-All Origins",
               State == "United States")
    } else if (input$groupBy == "Sex") {
      filtered <- dataNCHS %>%
        filter(Race.and.Hispanic.Origin == input$raceInput,
               Age.Group == input$agegroupInput,
               Sex != "Both Sexes",
               State == "United States")
    }
    
    group_by_factor <- switch(input$groupBy,
                              "Age Group" = "Age.Group",
                              "Race" = "Race.and.Hispanic.Origin",
                              "Sex" = "Sex")
    
    if (nrow(filtered) == 0) {
      ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "No data available for the selected filters"), size = 6, color = "red") +
        theme_void()
    } else {
      total_deaths_by_year <- dataNCHS %>%
        filter(State == "United States",
               Age.Group == "All Ages",
               Sex == "Both Sexes",
               Race.and.Hispanic.Origin == "All Races-All Origins") %>%
        select(Year, Deaths)
      
      # Plotting data and total deaths
      ggplot() +
        geom_bar(data = filtered, aes_string(x = "Year", y = "Deaths", fill = group_by_factor), position = "stack", stat = "identity") +
        geom_line(data = total_deaths_by_year, aes(x = Year, y = Deaths, color = "Total Nationwide Mortality"), size = 1.2) + 
        labs(title = "Opioid Deaths in the United States", x = "Year", y = "Deaths") +
        scale_fill_brewer(palette = "Set3") +  
        scale_color_manual(values = c("Total Nationwide Mortality" = input$lineColor), 
                           labels = "Total Nationwide Mortality") + 
        theme_economist() +
        theme(legend.position = "bottom")
    }
  })
  
  # Render the data table in the second tab
  output$data_table <- renderDT({
    # Filter data based on the selected grouping
    if (input$groupBy == "Age Group") {
      filtered <- dataNCHS %>%
        filter(Race.and.Hispanic.Origin == input$raceInput,
               Sex == input$sexInput,
               Age.Group != "All Ages",
               State != "United States")  # Excluding US total
    } else if (input$groupBy == "Race") {
      filtered <- dataNCHS %>%
        filter(Age.Group == input$agegroupInput,
               Sex == input$sexInput,
               Race.and.Hispanic.Origin != "All Races-All Origins",
               State != "United States")  # Excluding US total
    } else if (input$groupBy == "Sex") {
      filtered <- dataNCHS %>%
        filter(Race.and.Hispanic.Origin == input$raceInput,
               Age.Group == input$agegroupInput,
               Sex != "Both Sexes",
               State != "United States")  # Excluding US total
    }
    
    # Select relevant columns to display: State, Age Group, Race, Sex
    filtered_table <- filtered %>%
      select(State, Age.Group, Race.and.Hispanic.Origin, Sex, Deaths)
    
    # Return the data table with custom styling
    datatable(filtered_table, 
              options = list(pageLength = 10, 
                             autoWidth = TRUE, 
                             dom = 't',  # Hide table controls
                             columnDefs = list(list(targets = 0, width = '20%'),
                                               list(targets = 1:4, width = '15%'))),  # Adjust column widths
              class = 'display') %>%  # Ensure table is styled with default display class
      formatStyle(
        columns = 1:5,  # Apply styling to all columns
        backgroundColor = 'white'  # Set background color to white
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

