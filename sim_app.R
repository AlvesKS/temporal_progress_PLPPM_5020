sim_app = function(){

library(shiny)
library(ggplot2)
  runApp(

    

shinyApp(
  
  ui = fluidPage(
    
    
    
    column(width = 3,
           selectInput(inputId = "model",
                       label = "Select the model",
                       multiple = T,
                       choices = c("Exponential", "Monomolecular", "Logistic", "Gompertz") ),
           
           sliderInput(inputId = "r",
                       label = "Apparent infection rate",
                       min = 0.0001,
                       max= 0.9,
                       value = 0.05),
           
           sliderInput(inputId = "y0",
                       label = "Initial inoculum",
                       min = 0.001,
                       max= 0.1,
                       value = 0.02),
           
           sliderInput(inputId = "K",
                       label = "Maximum asymptote",
                       min = 0.1,
                       max= 1,
                       value = 1)
    ),
    column(width = 9,
           
           plotOutput("Plot")
    )
    
  ),
  
  server = function(input, output) {
    output$Plot = renderPlot({
      
      exp_data = epifitter::sim_exponential(
        N = 60,
        dt = 0.5,
        y0 = input$y0,
        r = input$r,
        # K = input$K,
        n = 1,
        alpha = 0.1
      ) %>% 
        mutate(model = "Exponential")
      
      mono_data = epifitter::sim_monomolecular(
        N = 60,
        dt = 0.5,
        y0 = input$y0,
        r = input$r,
        K = input$K,
        n = 1,
        alpha = 0.1
      )%>% 
        mutate(model = "Monomolecular")
      
      logi_data = epifitter::sim_logistic(
        N = 60,
        dt = 0.5,
        y0 = input$y0,
        r = input$r,
        K = input$K,
        n = 1,
        alpha = 0.1
      )%>% 
        mutate(model = "Logistic")
      
      gompi_data = epifitter::sim_gompertz(
        N = 60,
        dt = 0.5,
        y0 = input$y0,
        r = input$r,
        K = input$K,
        n = 1,
        alpha = 0.1
      )%>% 
        mutate(model = "Gompertz")
      
      
      
     bind_rows(gompi_data,logi_data,mono_data,exp_data) %>% 
       filter(model %in% input$model) %>%
       filter(y<=1) %>% 
        ggplot(aes(time, y, color = model)) +
        # geom_point(size = 2)+
        geom_line(size =1)+
        ylim(0,1)+
       xlim(0,60)+
       ggthemes::scale_color_colorblind()+
        cowplot::theme_half_open()
      
    })
  }
  
  # options = list(height = 500)
)
)
}