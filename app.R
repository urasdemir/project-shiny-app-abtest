library(shiny)

ui <- fluidPage(
  titlePanel("A/B Testing Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Description"),
      p("This application simulates an A/B test where you can input the conversion rates and the number of visitors for two different versions (A and B). 
        The app will then simulate conversion successes and perform a statistical test to compare the two versions. 
        The results are displayed both as text and in graphical form."),
      
      numericInput("convA", "Conversion Rate for A (%)", value = 5, min = 0, max = 100),
      numericInput("convB", "Conversion Rate for B (%)", value = 5, min = 0, max = 100),
      numericInput("nA", "Number of Visitors for A", value = 1000, min = 1),
      numericInput("nB", "Number of Visitors for B", value = 1000, min = 1),
      
      actionButton("run", "Run A/B Test"),
      
      h4("Test Explanation"),
      p("The Proportion Test is used to compare two proportions. In the context of A/B testing, it compares the conversion rates (proportions of visitors who convert) of two versions. This test is suitable for binary outcomes (e.g., converted or not converted)."),
      
      h4("Why Results Vary"),
      p("The results of each test can vary with each run because the app simulates conversion data using random sampling. Each simulation can yield slightly different conversion rates due to the inherent randomness in the sampling process. This variability is a natural part of statistical simulation and helps illustrate the concept of sampling variability."),
      
      h4("Interpreting the Outcome"),
      p("The p-value indicates the probability that the observed difference in conversion rates occurred by chance. If the p-value is less than 0.05, it suggests that there is a statistically significant difference between the two versions. Otherwise, it suggests that any observed difference is likely due to random variation.")
    ),
    
    mainPanel(
      h3("Results"),
      verbatimTextOutput("results"),
      h3("Conversion Rate Comparison"),
      plotOutput("conversionPlot"),
      
      hr(),
      p("Shiny app by Uras Demir", style = "text-align: center;")
    )
  )
)

library(shiny)
library(ggplot2)

server <- function(input, output) {
  observeEvent(input$run, {
    convA <- input$convA / 100
    convB <- input$convB / 100
    nA <- input$nA
    nB <- input$nB
    
    # Simulate conversion data
    successesA <- rbinom(1, nA, convA)
    successesB <- rbinom(1, nB, convB)
    
    propA <- successesA / nA
    propB <- successesB / nB
    
    # Perform proportion test
    test_result <- prop.test(c(successesA, successesB), c(nA, nB))
    
    output$results <- renderPrint({
      cat("Conversion Rate for A: ", round(propA * 100, 2), "%\n")
      cat("Conversion Rate for B: ", round(propB * 100, 2), "%\n\n")
      cat("p-value: ", test_result$p.value, "\n")
      
      if (test_result$p.value < 0.05) {
        cat("Result: Significant difference\n")
      } else {
        cat("Result: No significant difference\n")
      }
    })
    
    output$conversionPlot <- renderPlot({
      df <- data.frame(
        Version = c("A", "B"),
        ConversionRate = c(propA, propB)
      )
      
      ggplot(df, aes(x = Version, y = ConversionRate * 100, fill = Version)) +
        geom_bar(stat = "identity") +
        ylim(0, 100) +
        ylab("Conversion Rate (%)") +
        ggtitle("Conversion Rates for A and B") +
        theme_minimal()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

