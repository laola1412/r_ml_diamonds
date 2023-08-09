library("ggplot2")
library(dplyr)
library(ggthemes)
library(ggridges)
library(caret)
data(diamonds)
library(shiny)
library(bslib)

options(scipen = 999)

corrected_diamonds <- diamonds %>%
    mutate(size = x * y * z) %>%
    select(carat, size, price, cut, color, depth)

p <- ggplot(corrected_diamonds, aes(price, cut, fill = stat(x))) +
    geom_density_ridges_gradient(scale = 3) +
    scale_fill_viridis_c(name = "Price", option = "C") +
    labs(title = "Prices of Different Diamond Cuts")

ggsave("diamonds.png", plot = p, width = 10, height = 10)

# density plot feature impact comparison
# p2 <- featurePlot(
#     x = corrected_diamonds[, c("depth", "size", "carat")],
#     y = corrected_diamonds$cut,
#     plot = "density",
#     layout = c(3, 1),
#     scales = list(
#         x = list(relation = "free"),
#         y = list(relation = "free")
#     ),
#     adjust = 1.5,
#     auto.key = list(columns = 3)
# )

# print(p2)
# trellis.device("png", file = "testcaret3.png", width = 1500, height = 800)
# print(p2)

# dev.off()

# boxplot feature comparison
# p3 <- featurePlot(
#     x = corrected_diamonds[, c("depth", "size", "carat")],
#     y = corrected_diamonds$cut,
#     plot = "box",
#     layout = c(3, 1),
#     scales = list(
#         y = list(relation = "free"),
#         x = list(rot = 90)
#     ),
#     adjust = 1.5,
#     auto.key = list(columns = 3)
# )

# # correlation plots
# p4 <- featurePlot(
#     x = corrected_diamonds[, c("depth", "size", "carat")],
#     y = corrected_diamonds$price,
#     plot = "scatter",
#     layout = c(3, 1),
#     type = c("p", "smooth"),
#     span = .5
# )
# print(p4)

# # Load the diamonds dataset
# data("diamonds")

# UI
ui <- page_sidebar(
    theme = bs_theme(bootswatch = "minty"),
    sidebar = sidebar(
        varSelectInput("xvar", "X variable", corrected_diamonds, selected = "Bill Length (mm)"),
        varSelectInput("yvar", "Y variable", corrected_diamonds, selected = "Bill Depth (mm)"),
        checkboxGroupInput(
            "species", "Filter by species",
            choices = unique(corrected_diamonds$cut),
            selected = unique(corrected_diamonds$cut)
        ),
        hr(), # Add a horizontal rule
        checkboxInput("by_species", "Show species", TRUE),
        checkboxInput("show_margins", "Show marginal plots", TRUE),
        checkboxInput("smooth", "Add smoother"),
    ),
    plotOutput("scatter")
)

server <- function(input, output, session) {
    # Reactive expression to filter the data based on selected feature
    subsetted <- reactive({
        req(corrected_diamonds$cut)
        df |> filter(Cut %in% corrected_diamonds$cut)
    })

    # Render the scatter plot
    output$scatter <- renderPlot(
        {
            # Create the base scatter plot
            p <- ggplot(subsetted(), aes(!!corrected_diamonds$carat, !!corrected_diamonds$price)) +
                list(
                    theme(legend.position = "bottom"), # Set legend position at the bottom
                    if (input$by_species) aes(color = cut), # Color points by species if selected
                    geom_point(), # Add scatter points
                    if (input$smooth) geom_smooth() # Add a smoother if selected
                )

            # Add marginal plots if the checkbox is selected
            if (input$show_margins) {
                margin_type <- if (input$by_species) "density" else "histogram"
                p <- ggExtra::ggMarginal(p,
                    type = margin_type, margins = "both",
                    size = 8, groupColour = input$by_species, groupFill = input$by_species
                )
            }

            p # Return the final plot
        },
        res = 100
    ) # Set the resolution of the plot
}

# Run the Shiny app with the defined UI and server functions
shinyApp(ui, server)
