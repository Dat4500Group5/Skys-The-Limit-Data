library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Aircraft Delay & Cancellation Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("plot_type", "Plot Type:",
                   choices = c("Scatterplot (one year)" = "scatter",
                               "Line Plot (averages by year)" = "line"),
                   selected = "scatter"),
      conditionalPanel(
        condition = "input.plot_type == 'scatter'",
        sliderInput("year", "Select Year:",
                    min = 2003, max = 2024, value = 2020, step = 1, sep = "")
      ),
      selectInput("xvar", "Select X Variable:",
                  choices = c("Aircraft Age" = "AGE",
                              "Flight Count" = "FLIGHTS",
                              "Distance" = "DISTANCE",
                              "Air Time" = "AIR_TIME"),
                  selected = "DISTANCE"),
      selectInput("yvar", "Select Y Variable:",
                  choices = c(
                    "Carrier Delay Ratio (>30 min)" = "COUNT_CARRIER_DELAY_30_RATIO",
                    "Carrier Delay Count (>30 min)" = "COUNT_CARRIER_DELAY_30",
                    "Late Aircraft Delay (>30 min)" = "LATE_AIRCRAFT_DELAY_30",
                    "Late Aircraft Delay Count (>30 min)" = "COUNT_LATE_AIRCRAFT_DELAY_30",
                    "Carrier Cancellations" = "COUNT_CARRIER_CANCELLATION",
                    "Late Aircraft Delay Ratio" = "COUNT_LATE_AIRCRAFT_DELAY_30_RATIO"
                  ),
                  selected = "COUNT_CARRIER_DELAY_30_RATIO")
    ),
    
    mainPanel(
      plotOutput("mainPlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$mainPlot <- renderPlot({
    if (input$plot_type == "scatter") {
      # Scatterplot filtered by selected year
      filtered_data <- Ratios_2 %>% filter(YEAR == input$year)
      
      # Fit linear model
      formula <- as.formula(paste(input$yvar, "~", input$xvar))
      model <- lm(formula, data = filtered_data)
      coef <- coef(model)
      
      # Format regression equation text
      intercept <- round(coef[1], 4)
      slope <- round(coef[2], 10)
      eq_label <- paste0("y = ", intercept,
                         ifelse(slope >= 0, " + ", " - "),
                         abs(slope), "x")
      
      ggplot(filtered_data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        annotate("text",
                 x = min(filtered_data[[input$xvar]], na.rm = TRUE),
                 y = max(filtered_data[[input$yvar]], na.rm = TRUE),
                 label = eq_label,
                 hjust = 0, vjust = 1.2,
                 size = 5, color = "black") +
        labs(
          x = input$xvar,
          y = input$yvar,
          title = paste(input$xvar, "vs.", input$yvar, "in", input$year)
        ) +
        theme_minimal()
      
    } else {
      # Line plot of averages over years
      summary_data <- Ratios_2 %>%
        group_by(YEAR) %>%
        summarize(
          avg_x = mean(.data[[input$xvar]], na.rm = TRUE),
          avg_y = mean(.data[[input$yvar]], na.rm = TRUE)
        )
      
      ggplot(summary_data, aes(x = avg_x, y = avg_y)) +
        geom_line(color = "darkred", size = 1.2) +
        geom_point(color = "black") +
        labs(
          x = paste("Average", input$xvar),
          y = paste("Average", input$yvar),
          title = paste("Trend of Average", input$yvar, "vs.", input$xvar, "(2003–2024)")
        ) +
        theme_minimal()
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

