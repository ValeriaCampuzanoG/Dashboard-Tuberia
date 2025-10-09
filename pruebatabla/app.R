library(shiny)
library(dplyr)
library(reactable)
#install.packages("reactablefmtr")
library(reactablefmtr)
library(writexl)
#install.packages("webshot2")
library(webshot2)

# Fake dataset
set.seed(123)
data <- expand.grid(
  state = c("State 1", "State 2", "State 3"),
  variable = c("Variable A", "Variable B"),
  year = 2019:2023
)
data$total <- sample(50:500, nrow(data), replace = TRUE)

# UI
ui <- fluidPage(
  titlePanel("Custom Table Generator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_state", "Select State(s):", 
                  choices = unique(data$state), multiple = TRUE, selected = "State 1"),
      selectInput("selected_variable", "Select Variable(s):", 
                  choices = unique(data$variable), multiple = TRUE, selected = "Variable A"),
      sliderInput("selected_years", "Select Year(s):",
                  min = min(data$year), max = max(data$year),
                  value = c(min(data$year), max(data$year)), step = 1),
      downloadButton("download_xlsx", "Download XLSX"),
      downloadButton("download_png", "Download PNG")
    ),
    
    mainPanel(
      h3(textOutput("table_title")),
      reactableOutput("custom_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data %>%
      filter(
        state %in% input$selected_state,
        variable %in% input$selected_variable,
        year >= input$selected_years[1],
        year <= input$selected_years[2]
      )
  })
  
  # Reactive for table construction
  custom_table_data <- reactive({
    df <- filtered_data()
    
    # Case 1: One state, one variable
    if(length(input$selected_state) == 1 && length(input$selected_variable) == 1){
      df %>%
        select(year, total) %>%
        rename(Año = year, Total = total)
      
      # Case 2: One state, multiple variables
    } else if(length(input$selected_state) == 1 && length(input$selected_variable) > 1){
      df %>%
        select(state, variable, year, total) %>%
        tidyr::pivot_wider(names_from = variable, values_from = total) %>%
        rename(Año = year)
      
      # Case 3: Multiple states, one variable
    } else if(length(input$selected_state) > 1 && length(input$selected_variable) == 1){
      df %>%
        select(state, variable, year, total) %>%
        tidyr::pivot_wider(names_from = state, values_from = total) %>%
        rename(Año = year)
      
    } else {
      # fallback: just show year + total
      df %>%
        group_by(year) %>%
        summarise(Total = sum(total)) %>%
        rename(Año = year)
    }
  })
  
  # Table title
  output$table_title <- renderText({
    df <- filtered_data()
    years <- range(df$year)
    
    if(length(input$selected_state) == 1 && length(input$selected_variable) == 1){
      paste0("Total de ", input$selected_variable, " en ", input$selected_state, ", ", 
             years[1], "-", years[2])
    } else if(length(input$selected_state) == 1 && length(input$selected_variable) > 1){
      paste0("Indicadores procesales de ", input$selected_state, ", ", 
             years[1], "-", years[2])
    } else if(length(input$selected_state) > 1 && length(input$selected_variable) == 1){
      paste0("Evolución de ", input$selected_variable, ", ", years[1], "-", years[2])
    } else {
      paste0("Resumen ", years[1], "-", years[2])
    }
  })
  
  # Render reactable
  output$custom_table <- renderReactable({
    reactable(custom_table_data(), searchable = TRUE, filterable = TRUE)
  })
  
  # Download XLSX
  output$download_xlsx <- downloadHandler(
    filename = function() {"custom_table.xlsx"},
    content = function(file) {
      write_xlsx(custom_table_data(), file)
    }
  )
  
  # Download PNG
  output$download_png <- downloadHandler(
    filename = function() {"custom_table.png"},
    content = function(file) {
      tmpfile <- tempfile(fileext = ".html")
      reactablefmtr::save_reactable(custom_table_data(), tmpfile)
      webshot2::webshot(tmpfile, file = file, vwidth = 1200, vheight = 800)
    }
  )
  
}

# Run app
shinyApp(ui, server)

