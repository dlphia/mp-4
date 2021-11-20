library(shiny)
library(shinyjs)
library(shinystan)
library(shinythemes)

shinyUI
(
  fluidPage
  ( theme = shinytheme("cerulean"),
    headerPanel("Heart Disease Prediction"),
    sidebarLayout
    (
      sidebarPanel
      (
        wellPanel
        (
          helpText(HTML("<b>Welcome</b>")),
          HTML(""),
            submitButton("Submit")
        ),
        
        wellPanel
        (
          helpText(HTML("<b>Age</b>")),
          selectInput("Age","",
                      choices=""),
          helpText("Examples: 50"),
         
        ),
        
        wellPanel
        (
          helpText(HTML("<b>Gender</b>")),
          selectInput("Gender","",
                      choices=""),
         helpText("Examples: M, F"), 
        ),
        
        wellPanel
        (
          helpText(HTML("<b>Cholesterol levels</b>")),
          selectInput("Chol","",
                      choices=""),
         helpText("Examples: mg/dL"), 
        ),
        
        wellPanel
        (
          helpText(HTML("<b>Thalach</b>")),
          selectInput("Thalach","maximum heart rate achieved",
                      choices=""),
          #helpText("Examples: "), 
        )

      ),
     
      mainPanel
      (
        tabsetPanel(
          type = "tab",
          tabPanel("Age",plotOutput("age_")),
          tabPanel("Cholesterol levels",plotOutput("Chol_")),
          tabPanel("Constrictive pericarditis (CP) ",plotOutput("cp")),
          tabPanel("Thalach",plotOutput("t_op"))

          
          
        )
      )
    ) 
    
    
  )
)

