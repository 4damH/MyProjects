# Task outline:
# Build a shiny application which reads in one of the weather datasets provided
# with the course and plots the variables.
# Users should be able to choose the variables to plot and the years to plot
# Additional credit will be given if the user is able to choose more than one
# dataset to investigate.


# load packages
library(shiny)
library(tidyverse)
library(plotly)

# read data in
# I'll use one of the dataset cleaned already in Exercises_pt8
# Please change the filepath if necessary
weather_df <- read_csv("C:/Users/adamh/Documents/10 FolderFor_R/Intro_to_R_Data_Vis/Assessments/AH_Shiny_assessment/Braemar_AH_shiny.csv")
variable_to_choose <- weather_df %>%
          select(3:7)


# Define what the user sees
ui <- navbarPage(
  title = "Weather data",
  tabPanel(
    title = "App made by Adam Hudzik",
  
  # Sidebar with inputs for user to control 
  sidebarLayout(
    sidebarPanel(
      # add instructions
      p("Please make your choices from the menus below and then press `Plot Data`"),
      
      # choose a year
      selectizeInput(label = "Year (one or multiple)",
                  inputId = "year", 
                  choices = sort(unique(weather_df$yyyy)), # choices are sorted
                  selected = sort(unique(weather_df$yyyy)),
                  multiple = TRUE), 
      # choose a variable to plot
      selectInput(label = "Variables (one or multiple)",
                  inputId = "var_to_plot", 
                  choices = names(variable_to_choose), # choices are sorted
                  selected = c("tmin (degC)", "tmax (degC)"),
                  multiple = TRUE),
      # confirm your choices
      actionButton(inputId = "go", label = "Plot Data")
    ),
    
    # Show the generated plots
    mainPanel(
      
      tabsetPanel(
        tabPanel("Data visualisation", plotlyOutput("plot")),
        tabPanel("Data table", dataTableOutput("table"))
        
      )
    )
  ))
)

# Define server logic required to sort data and draw plot
server <- function(input, output, session) { 
  
  sample_weather <- reactive({
    
    # isolate user choices
    chosen_years <- isolate(input$year) # chosen year 
    chosen_variables <- isolate(input$var_to_plot) # chosen variables
    
    # listen to go button
    input$go
    
    # filter data to plot, selected by user 
    weather_filtered <- weather_df %>%
      filter(yyyy %in% chosen_years) %>%
      select(yyyy, date, chosen_variables)
    
    # create pivot longer for plotting that includes chosen variables
    weather_to_plot <- pivot_longer(weather_filtered,
                                    cols = chosen_variables,
                                    names_to = "type_of_weather_data",
                                    values_to = "values")
    
    return(weather_to_plot) 
  })
  
  # create a plot - line graph (type="scatter", mode="lines+markers") for chosen variables
  # and modify the layout using pipe %>%
  output$plot <- renderPlotly({
    data_to_plot <- sample_weather()
    plot_ly(data = data_to_plot,
            x = ~date,
            y = ~values, 
            color = ~type_of_weather_data,
            linetype = ~type_of_weather_data,
            type = 'scatter', 
            mode = 'lines+markers') %>% 
              layout(title = isolate(input$var_to_plot), # this is displayed only for one chosen variable
                     xaxis = list(title = '<b> Date </b>'),
                     yaxis = list(title = '<b> Values </b>'),
                     legend = list(title = list(text = "<b> Selected variables </b>")))
  })

  # display a table with chosen data
  output$table <- renderDataTable({ 
    data_to_plot <- sample_weather()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
