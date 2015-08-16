
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

PredictChildHeight <- function(Father_h, Mother_h, Gender_c)
{
  #Data Processing
  Data <- read.csv("D:/Projects/Analytics/CourseEra/DataScience/09_DevelopingDataProducts/Proj/Galton.csv",  header=TRUE) # stringsAsFactors=FALSE, , na.strings = "?", dec = "."
  
  #Creating prediction function
  fit <- glm(Height ~ Mother+ Father  + Gender -1 , data = Data)

  Father_h <- as.numeric(Father_h)
  Mother_h <- as.numeric(Mother_h)
  
  predData <- data.frame(Mother = Mother_h, Father = Father_h, Gender = Gender_c)
  
  predResult <- predict(fit, newdata = predData)
  
  return(predResult)
}

shinyServer(function(input, output) {
  
  output$Father_o <- renderText({
    if(input$btnPredictChildHeight > 0) input$Father
    else ""
  })
  
  output$Mother_o <- renderText({
    if(input$btnPredictChildHeight > 0) input$Mother
    else ""
  })
  
  output$Gender_o <- renderText({
    if(input$btnPredictChildHeight > 0) input$Gender
    else ""
  })
  
  output$child_pred_o <- renderText({
    if(input$btnPredictChildHeight > 0) PredictChildHeight(input$Father, input$Mother, input$Gender)
    else ""
  })
  
  
}) # shinyServer