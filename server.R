shinyServer
(
  
  function(input, output) 
  {
    output$cp <- renderPlot({
      
      plot(cp)
      
      
    })
    
    output$fate <- renderPlot({
      
      plot(fate)
      
      
    })
    
    output$Chol_ <- renderPlot({
      
      plot(Chol_)
      
      
    })
    
    output$t_op <- renderPlot({
      plot(t_op)
      
      
    })
    output$age_ <- renderPlot({
      
      plot(age_)
      
      
    })
    
    
    
  }
)

