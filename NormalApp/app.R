ui <- fluidPage(
  fluidRow(
    column(2, numericInput(inputId = "MU", label = "Mean:", value = 0, min = -Inf, max = Inf)),
    column(2, numericInput(inputId = "SIGMA", label = "Std:", value = 1, min = 0.0000001, max = Inf)),
    column(2, numericInput(inputId = "MIN", label = "Min:", value = -2, min = -Inf, max = Inf)),
    column(2, numericInput(inputId = "MAX", label = "Max:", value = 2, min = -Inf, max = Inf)),
    column(8, offset = 2, plotOutput("NG", height = "500px", width = "500px"))
  ))


server <- function(input, output){
  library(ggplot2)
  output$NG <- renderPlot({
    abN <- round(pnorm(input$MAX, input$MU, input$SIGMA) - pnorm(input$MIN, input$MU, input$SIGMA),4) 
    p <- ggplot(data.frame(x=c(input$MU - 3.5*input$SIGMA, input$MU + 3.5*input$SIGMA)), aes(x = x)) 
    limitRange <- function(min = input$MIN, max = input$MAX) {
      function(x) {
        y <- dnorm(x, input$MU, input$SIGMA)
        y[x < input$MIN | x > input$MAX] <- NA
        return(y)
      }
    }
    SDB <- c(-3, -2, -1, 0, 1, 2, 3)*input$SIGMA + input$MU
    p + stat_function(fun = dnorm, args = list(input$MU, input$SIGMA)) +
      stat_function(fun = limitRange(min = input$MIN, max = input$MAX),
                    geom = "area", fill = "blue", alpha = 0.4, n = 500) + 
      theme_bw() + 
      labs(x = paste("X ~ N(", input$MU,",",input$SIGMA,")"), y = "", title = paste("The area between", input$MIN, "and", input$MAX, "is", abN)) +
      scale_x_continuous(breaks=SDB)
  })
}
shinyApp(ui = ui, server = server)