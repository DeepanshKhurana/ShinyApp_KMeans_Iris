#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

data <- iris
data.new <- data[, c(1:4)]
data.class <- data[, 5]

normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}

data.new <- data.frame(sapply(data.new, normalize))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel("K-Means Clustering in Iris Dataset"),
   
   # Sidebar with a slider input for number of clusters
   sidebarLayout(
      sidebarPanel(
        
         HTML("<h4>This is a simple Shiny app that helps you visualise the K-Means clustering for the <b> Iris Dataset. </b> </h4> <br> <br>"),
         
         sliderInput("clusters",
                     "Number of clusters:",
                     min = 1,
                     max = 15,
                     value = 5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("sepalCluster"),
         plotOutput("petalCluster")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  clustering <- reactive(kmeans(data.new, input$clusters))
  
  output$sepalCluster <- renderPlot({
    
    palette(c("#f44336", "#2196f3", "#009688", "#4caf50",
              "#ffc107", "#ff9800", "#795548", "#607d8b", "#999999"))
    
    plot(data.new[c(1,2)], col=clustering()$cluster, pch = 20, cex = 1.5, xlab = "Sepal Length", ylab = "Sepal Width", main = "Sepal Length vs Sepal Width")
  
    points(clustering()$centers, pch = 4, cex = 1, lwd = 3)
    
  })
  
  output$petalCluster <- renderPlot({
    
    plot(data.new[c(3,4)], col=clustering()$cluster, pch = 20, cex = 1.5, xlab = "Petal Length", ylab = "Petal Width", main = "Petal Length vs Petal Width")
    
    points(clustering()$centers, pch = 4, cex = 1, lwd = 3)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

