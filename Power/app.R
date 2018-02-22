# Alan T. Arnholt 2/21/18
# Power app
# Load the shiny package
library(shiny)
library(ggplot2)
##### Function Defined:
powerg <- function(n = 36, mu0 = 30, mu1 = 31, sd = 6, alpha = 0.10, alternative = c("two.sided", "less", "greater"),
                   variance = c("unknown", "known"))
{
  
delta <- mu1 - mu0
gamma <- (delta)/(sd/sqrt(n))
  
variance <- match.arg(variance)
  
if(variance == "unknown" & gamma < 0){
    ll <- -5*sqrt((n/(n - 2))) + gamma*2
    ul <- 5*sqrt((n/(n - 2)))
    } else if (variance == "unknown" & gamma >= 0) { 
    ll <- -5*sqrt((n/(n - 2)))
    ul <- 5*sqrt((n/(n - 2))) + gamma*2
    } 
  
if (variance == "known") {
    ll <- min(mu0, mu1) - 3.4*sd/sqrt(n)
    ul <- max(mu0, mu1) + 3.4*sd/sqrt(n)
    }

p <- ggplot(data = data.frame(x = c(ll, ul)), aes(x = x))

alternative <- match.arg(alternative)
  
  if(alternative == "less" & variance == "unknown"){
    dt_fun1 <- function(x){
      y <- dt(x, n - 1)
      y[x > qt(alpha, n - 1)] <- NA
      return(y)
    }
  
    dt_fun2 <- function(x){
      y <- dt(x, n - 1, gamma)
      y[x > qt(alpha, n - 1)] <- NA
      return(y)
    }
    
    POWER <- round(pt(qt(alpha, n - 1), n-1, gamma), 4)
    
    } else if (alternative == "greater" & variance == "unknown") {
    dt_fun1 <- function(x){
      y <- dt(x, n - 1)
      y[x < qt(1 - alpha, n - 1)] <- NA
      return(y)
    }
    
    dt_fun2 <- function(x){
      y <- dt(x, n - 1, gamma)
      y[x < qt(1 - alpha, n - 1)] <- NA
      return(y)
    }
    
    POWER <- round(pt(qt(1- alpha, n - 1), n-1, gamma, lower.tail = FALSE), 4)
    
    } else if (alternative == "two.sided" & variance == "unknown") {
    dt_fun1 <- function(x){
      y <- dt(x, n - 1)
      y[x > qt(alpha/2, n - 1) & x < qt(1 - alpha/2, n - 1)] <- NA
      return(y)
    }
    
    dt_fun2 <- function(x){
      y <- dt(x, n - 1, gamma)
      y[x > qt(alpha/2, n - 1) & x < qt(1 - alpha /2, n - 1)] <- NA
      return(y)
    } 
    
    POWER <- round(pt(qt(alpha/2, n-1), n-1, gamma) + pt(qt(1- alpha/2, n - 1), n-1, gamma, lower.tail = FALSE), 4)
  }

########### Next Normal  

  if(alternative == "less" & variance == "known"){
    dnorm_fun1 <- function(x){
      y <- dnorm(x, mu0, sd/sqrt(n))
      y[x > qnorm(alpha, mu0, sd/sqrt(n))] <- NA
      return(y)
    }

    dnorm_fun2 <- function(x){
      y <- dnorm(x, mu1, sd/sqrt(n))
      y[x > qnorm(alpha, mu0, sd/sqrt(n))] <- NA
      return(y)
    }

    POWER <- round(pnorm(qnorm(alpha, mu0, sd/sqrt(n)), mu1, sd/sqrt(n)), 4)

    } else if (alternative == "greater" & variance == "known"){

    dnorm_fun1 <- function(x){
      y <- dnorm(x, mu0, sd/sqrt(n))
      y[x < qnorm(1 - alpha, mu0, sd/sqrt(n))] <- NA
      return(y)
    }

    dnorm_fun2 <- function(x){
      y <- dnorm(x, mu1, sd/sqrt(n))
      y[x < qnorm(1 - alpha, mu0, sd/sqrt(n))] <- NA
      return(y)
    }

    POWER <- round(pnorm(qnorm(1- alpha, mu0, sd/sqrt(n)), mu1, sd/sqrt(n), lower.tail = FALSE), 4)

    } else if (alternative == "two.sided" & variance == "known") {
    dnorm_fun1 <- function(x){
      y <- dnorm(x, mu0, sd/sqrt(n))
      y[x > qnorm(alpha/2, mu0, sd/sqrt(n)) & x < qnorm(1 - alpha/2, mu0, sd/sqrt(n))] <- NA
      return(y)
    }

    dnorm_fun2 <- function(x){
      y <- dnorm(x, mu1, sd/sqrt(n))
      y[x > qnorm(alpha/2, mu0, sd/sqrt(n)) & x < qnorm(1 - alpha /2, mu0, sd/sqrt(n))] <- NA
      return(y)
    }

    POWER <- round(pnorm(qnorm(alpha/2, mu0, sd/sqrt(n)), mu1, sd/sqrt(n)) + pnorm(qnorm(1- alpha/2, mu0, sd/sqrt(n)), mu1, sd/sqrt(n), lower.tail = FALSE), 4)
}


if(variance == "unknown"){
p + stat_function(fun = dt_fun1, geom = "area", n = 500, fill = "red", alpha = 0.5) +
  stat_function(fun = dt_fun2, geom = "area", n = 500, fill = "blue", alpha = 0.5) +
  stat_function(fun = dt, args = list(n - 1), n = 500, color = "red") +
  stat_function(fun = dt, args = list(n - 1, gamma), n = 500, color = "blue") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x = "", y = "", title = paste0("Power ","(",POWER,") is the sum of all blue and purple shaded areas")) +
  theme(plot.title = element_text(hjust = 0.5))
} else {
p + stat_function(fun = dnorm_fun1, geom = "area", n = 500, fill = "red", alpha = 0.5) +
  stat_function(fun = dnorm_fun2, geom = "area", n = 500, fill = "blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mu0, sd/sqrt(n)), n = 500, color = "red") +
  stat_function(fun = dnorm, args = list(mu1, sd/sqrt(n)), n = 500, color = "blue") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x = "", y = "", title = paste0("Power ","(",POWER,") is the sum of all blue and purple shaded areas")) +
  theme(plot.title = element_text(hjust = 0.5))
}
}








#############
## Specify UI

# Define UI for the application
ui <- fluidPage(
  p(tags$strong("This app computes and depicts the power for a one-sample t-test (unknown variance) or z-test (known variance)")),
  tags$hr(""),
  withMathJax(),
  # Add a sidebar layout to the application
  sidebarLayout(
    # Add a sidebar panel around the text and inputs
    sidebarPanel(
      h3(em("Select Parameters")),
      radioButtons(inputId = "Type", label = "Select alternative hypothesis:", 
                   choices = c("\\(H_A:\\mu \\neq \\mu_0 \\)" = "two.sided",
                               "\\(H_A:\\mu > \\mu_0 \\)" = "greater",
                               "\\(H_A:\\mu < \\mu_0 \\)" = "less"),
                   selected = "two.sided"),
      radioButtons(inputId = "Variance", label = "Select appropriate button:", 
                   choices = c("Variance unknown" = "unknown",
                               "Variance known" = "known"),
                   selected = "unknown"),
      tags$hr(""),
      numericInput(inputId = "N", label = "Sample size (\\(n\\)):", value = 25, min = 3, max = 10000, step = 1),
      numericInput(inputId = "mu0", label = "Hypothesized mean \\( \\mu_0 \\):", value = 30, min = -100000, max = 100000, step = 1),
      numericInput(inputId = "mu1", label = "True mean \\( \\mu_1  \\):", value = 31, min = -100000, max = 100000, step = 1),
      numericInput(inputId = "Sigma", label = "Population standard deviation \\((\\sigma)\\):", value = 1, min = 0.001, max = 100000, step = 1),
      numericInput(inputId = "Alpha", label = "Significance level \\((\\alpha)\\):", value = 0.05, min = 0.001, max = 0.25, step = 0.001)
    ),
    # Add a main panel around the plot and table
    mainPanel(
      plotOutput("plot", height = "700px")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  output$plot <- renderPlot({
    powerg(n = input$N,
           sd = input$Sigma,
           mu0 = input$mu0,
           mu1 = input$mu1,
           alpha = input$Alpha,
           alternative = input$Type,
           variance = input$Variance)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
