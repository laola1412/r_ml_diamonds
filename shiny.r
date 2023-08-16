library(shiny)
library(ggplot2)

ui <- fluidPage(
    titlePanel("Comparable Companies Analysis"),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(
                    12,
                    sliderInput("y_scale", "Y-Axis Max", 50, min = 0, max = 1000)
                ),
                column(
                    12,
                    numericInput("current_sales", "Current 12m Trailing Sales:", 11563000000, min = 0, max = 1e9)
                ),
                column(
                    12,
                    numericInput("shares", "Shares Outstanding:", 388790000, min = 0, max = 1e9)
                ),
                column(
                    12,
                    numericInput("longgrowth", "Exp. 5y Sales Growth:", 0.05, min = -999, max = 999, step = 0.01)
                )
            ),
            fluidRow(
                column(
                    6,
                    numericInput("ev_sales_upper", "EV/Sales Upper:", 4, min = 0, max = 1e8, step = 0.01)
                ),
                column(
                    6,
                    numericInput("ev_sales_lower", "EV/Sales Lower:", 2, min = 0, max = 1e8, step = 0.01)
                ),
            )
        ),
        mainPanel(
            plotOutput("stock_plot")
        )
    )
)

server <- function(input, output) {
    output$stock_plot <- renderPlot({
        valuation_upper <- input$ev_sales_upper * (input$current_sales * (1 + input$longgrowth)^5) / input$shares
        valuation_lower <- input$ev_sales_lower * (input$current_sales * (1 + input$longgrowth)^5) / input$shares

        stock_dates <- seq(as.Date("2018-08-15"), as.Date("2028-08-15"), by = "month")

        past_prices <- seq(0.50, 0.94, length.out = length(stock_dates) - 61)
        future_prices_upper <- seq(0.94, valuation_upper, length.out = 61)
        future_prices_lower <- seq(0.94, valuation_lower, length.out = 61)
        prices <- c(past_prices, future_prices_upper)
        prices_lower <- c(past_prices, future_prices_lower)

        stock_data <- data.frame(date = stock_dates, price = prices)
        stock_data_lower <- data.frame(date = stock_dates, price = prices_lower)

        initial_price <- stock_data$price[61]
        final_price_upper <- stock_data$price[nrow(stock_data)]
        final_price_lower <- stock_data_lower$price[nrow(stock_data)]
        cagr_upper <- ((final_price_upper / initial_price)^(1 / 5) - 1) * 100
        cagr_lower <- ((final_price_lower / initial_price)^(1 / 5) - 1) * 100


        ggplot() +
            geom_line(data = stock_data[1:61, ], aes(x = date, y = price)) +
            geom_line(data = stock_data[61:121, ], aes(x = date, y = price), linetype = "dashed", colour = "blue") +
            geom_line(data = stock_data_lower[61:121, ], aes(x = date, y = price), linetype = "dashed", colour = "blue") +
            geom_text(
                data = stock_data[91, ],
                aes(x = date, y = price, label = paste("CAGR:", round(cagr_upper, 2), "%")),
                vjust = -2 # Vertical adjustment for the label position
            ) +
            geom_text(
                data = stock_data_lower[91, ],
                aes(x = date, y = price, label = paste("CAGR:", round(cagr_lower, 2), "%")),
                vjust = 2 # Vertical adjustment for the label position
            ) +
            geom_point(
                data = stock_data[61, ],
                aes(x = date, y = price, colour = "grey"), size = 3
            ) +
            geom_text(
                data = stock_data[61, ],
                aes(x = date, y = price, label = paste("Current Price:", round(price, 2), "$")),
                vjust = 2 # Vertical adjustment for the label position
            ) +
            geom_point(
                data = stock_data[121, ],
                aes(x = date, y = price, colour = "blue"), size = 3
            ) +
            geom_text(
                data = stock_data[121, ],
                aes(x = date, y = price, label = paste(round(price, 2), "$")),
                vjust = -1.2
            ) +
            geom_point(
                data = stock_data_lower[121, ],
                aes(x = date, y = price, colour = "blue"), size = 3
            ) +
            geom_text(
                data = stock_data_lower[121, ],
                aes(x = date, y = price, label = paste(round(price, 2), "$")),
                vjust = -1.2
            ) +
            labs(title = "Stock Price Prediction", x = "Year", y = "Stock Price in $") +
            theme(legend.position = "none") +
            ylim(c(0, input$y_scale))
    })
}

shinyApp(ui, server)
