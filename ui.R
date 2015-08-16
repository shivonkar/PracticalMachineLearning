library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("This projects takes data from Galton datset and build prediction model. It takes height of Father, Mother and Gender of the the child. Based on these input data the model is built using glm function. Inputs for predicting from Galton data."),
  
  sidebarPanel(
       textInput("Father", "Father's  Height", value = ""),
       textInput("Mother", "Mother's  Height", value = ""),
       radioButtons(inputId = "Gender", label = "Child's Gender", choices = c("Male" = "M", "Female" = "F")), #selected = "Male", inline = TRUE
       
       actionButton(inputId =  "btnPredictChildHeight", label = "Predict Child Height")

  ),
  mainPanel(
    h3('Illustrating outputs'),
    h4("You entered: Father's  Height"),
    verbatimTextOutput("Father_o"),
    h4("You entered: Mother's  Height"),
    verbatimTextOutput("Mother_o"),
    h4("You entered: Child Gender"),
    verbatimTextOutput("Gender_o"),
    
    h3("Predicted child height (from Galton data) is"),
    verbatimTextOutput("child_pred_o")
  )
))
