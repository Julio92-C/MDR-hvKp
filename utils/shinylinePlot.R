library(shiny)
library(ggplot2)

# Example data frame
data <- data.frame(
  Year = c(2000:2024),
  category1 = runif(25, 0, 100),
  category2 = runif(25, 0, 100),
  category3 = runif(25, 0, 100),
  category4 = runif(25, 0, 100)
)

# Define the UI
ui <- fluidPage(
  titlePanel("Prevalence Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("categories", 
                  "Select Categories", 
                  choices = colnames(data)[-1],
                  selected = " ",
                  multiple = TRUE)
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  
  output$linePlot <- renderPlot({
    # Check if any categories are selected
    if (length(input$categories) == 0) {
      # Return an empty plot if no categories are selected
      return(NULL)
    }
    
    
    # Filter the data based on the selected categories
    filteredData <- data.frame(data[, c("Year", input$categories)], stringsAsFactors = FALSE)
    
    # Convert the data to long format
    longData <- tidyr::gather(filteredData, "Category", "Prevalence", -Year)
    
    # Create the line plot using ggplot2
    ggplot(longData, aes(x = Year, y = Prevalence, color = Category)) +
      geom_line() +
      labs(x = "Year", y = "Prevalence", title = "Prevalence Over Time") +
      theme(legend.position = "right")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
