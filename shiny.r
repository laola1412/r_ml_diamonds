library(shiny)
library(ggplot2)

ui <- fluidPage(
    titlePanel("Comparable Companies Analysis"),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(
                    12,
                    numericInput("revenue", "Current Enterprise Value:", 1e6, min = 0, max = 1e9)
                ),
                column(
                    12,
                    numericInput("shares", "Shares Outstanding:", 1e6, min = 0, max = 1e9)
                )
            ),
            # fluidRow(
            #     column(
            #         6,
            #         numericInput(, "EV/EBITDA Upper:", 1e5, min = 0, max = 1e8)
            #     ),
            #     column(
            #         6,
            #         numericInput(, "EV/EBITDA Lower:", 20, min = 0, max = 50)
            #     )
            # )
            # fluidRow(
            #     column(
            #         6,
            #         numericInput(, "EV/FCF Upper:", 1e5, min = 0, max = 1e8)
            #     ),
            #     column(
            #         6,
            #         numericInput(, "EV/FCF Lower:", 20, min = 0, max = 50)
            #     )
            # )
            fluidRow(
                column(
                    6,
                    numericInput("net_income", "EV/Sales Upper:", 1e5, min = 0, max = 1e8)
                ),
                column(
                    6,
                    numericInput("pe_ratio", "EV/Sales Lower:", 20, min = 0, max = 50)
                )
            )
        ),
        mainPanel(
            h1("test1234"),
            plotOutput("stock_plot")
        )
    )
)

server <- function(input, output) {
    output$stock_plot <- renderPlot({
        valuation <- input$net_income * input$pe_ratio / input$shares

        stock_dates <- seq(as.Date("2018-08-15"), as.Date("2028-08-15"), by = "month")

        past_prices <- seq(20, 80, length.out = length(stock_dates) - 61)
        future_prices <- seq(80, 120, length.out = 60)
        prices <- c(past_prices, future_prices, valuation)

        stock_data <- data.frame(date = stock_dates, price = prices)

        ggplot(stock_data, aes(x = date, y = price)) +
            geom_line() +
            geom_line(data = stock_data[61:121, ], aes(linetype = "dotted")) +
            geom_point(
                data = stock_data[length(stock_data$date), ],
                aes(colour = "Predicted"), size = 3
            ) +
            labs(title = "Stock Price Prediction") +
            theme(legend.position = "none")
    })
}

shinyApp(ui, server)
