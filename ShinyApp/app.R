library(shiny)
library(tidyverse)
library(shinythemes)
library(ggthemes) 
library(shinyjs)
library(colourpicker)
library(DT)
library(rsconnect)

# Load in data
dataNCHS <- read.csv("https://raw.githubusercontent.com/stat545ubc-2024/assignment-b3-kjm6/refs/heads/main/NCHS_DrugPoisioningMortalityUnitedStates.csv")
#Clean and filter data
dataNCHS$Deaths <- as.numeric(gsub(",", "", dataNCHS$Deaths)) 
dataNCHS$Year <- as.numeric(dataNCHS$Year) 
dataNCHS <- dataNCHS %>% 
  select(c("State", "Year", "Sex", "Age.Group", "Race.and.Hispanic.Origin", "Deaths"))

# Define UI for the application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  titlePanel("US Overdose Mortality 1999 - 2016"),
    sidebarLayout(
    sidebarPanel(
      #FEATURE: This conditional sidebar panel is for the graphing tab. It includes a drop-down to select how bar-charts 
      #will be grouped, along with selecting other demographic features to filter by. If a filter set is selected 
      #which does not display available data, the output displays text that the data is not available.
      conditionalPanel(
        condition = "input.tabs == 'Plot of Nationwide Mortality by Demographic'",
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
      #FEATURE: This conditional sidebar panel is for the table tab. It includes a multiple selector to select states
      #to be visible in the table. It also includes a button to download a CSV of the current table format.
      conditionalPanel(
        condition = "input.tabs == 'Table of State-level Annual Mortality'",
        selectInput("stateInputforTable", "State",
                    choices = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
                                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                                "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
                                "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"),
                    selected = "Alabama",
                    multiple = TRUE
        ),
        downloadButton("downloadData", "Download State-level Data"),
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs", 
                  tabPanel("Plot of Nationwide Mortality by Demographic", 
                           plotOutput("plot")  
                  ),
                  tabPanel("Table of State-level Annual Mortality", 
                           DTOutput("data_table")
                  )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  #FEATURE: This code will disable the selection box for whichever demographic is selected in the group-by drop-down
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
      #FEATURE: This code prints a plot with a line graph showing total US mortality and a stacked barchart splitting mortality 
      #by selected demographic grouping
      ggplot() +
        geom_bar(data = filtered, aes_string(x = "Year", y = "Deaths", fill = group_by_factor), position = "stack", stat = "identity") +
        geom_line(data = total_deaths_by_year, aes(x = Year, y = Deaths, color = "Total Nationwide Mortality"), size = 1.2) + 
        labs(title = "Opioid Overdose Deaths in the United States", x = "Year", y = "Deaths") +
        scale_fill_brewer(palette = "Set3") +  
        scale_color_manual(values = c("Total Nationwide Mortality" = input$lineColor), 
                           labels = "Total Nationwide Mortality") + 
        theme_economist() +
        theme(legend.position = "bottom")
    }
  })
  #print the data table to the data table tab
  filtered_data <- reactive({
    dataNCHS %>%
      filter(State %in% input$stateInputforTable)
  })
  
  #FEATURE: This code prints a table showing data for the selected states from the input point.
  output$data_table <- renderDT({
    datatable(filtered_data(),
              options = list(
                pageLength = 25, 
                autoWidth = TRUE, 
                dom = 'lftip',
                filter = 'top'
              ),
              class = 'display')
  })
  
  #FEATURE: This code names and exports a file of the user's view of the data table as a CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("StateLevelOpioidMortality", paste(input$stateInputforTable, collapse = "_"), Sys.Date(), ".csv")
    },
    content = function(file) {
      # Write the filtered data to a CSV file
      write.csv(filtered_data(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)