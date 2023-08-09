source("diamonds.r")

library(shiny)
library(ggplot2)

# # Load the diamonds dataset
# data("diamonds")

# UI
ui <- fluidPage(
    titlePanel("Diamonds Dataset Exploration"),
    sidebarLayout(
        sidebarPanel(
            selectInput("x_var", "X-axis variable:", choices = names(diamonds)),
            selectInput("y_var", "Y-axis variable:", choices = names(diamonds)),
            sliderInput("sample_size", "Sample Size:", min = 10, max = nrow(diamonds), value = 100, step = 10)
        ),
        mainPanel(
            plotOutput("scatter_plot")
        )
    )
)

# Server
server <- function(input, output) {
    output$scatter_plot <- renderPlot({
        # Sample the dataset based on the chosen sample size
        sample_data <- diamonds[sample(nrow(diamonds), input$sample_size), ]

        # Create the scatter plot
        ggplot(sample_data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
            geom_point() +
            labs(x = input$x_var, y = input$y_var, title = "Scatter Plot of Diamonds Dataset")
    })
}

# Run the Shiny app
shinyApp(ui, server)
