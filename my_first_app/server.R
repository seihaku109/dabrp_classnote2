if (!require("gapminder")){install.packages("gapminder")}

library(gapminder)
str(gapminder)

library(gridExtra)
library(dplyr)
library(ggplot2)

function(input, output) {
  
  
  
  # You can access the values of the widget (as a vector)
  # with input$checkGroup, e.g.
  output$out1 <- renderPlot({
    
   gapminder %>% filter(country %in% input$con) %>%
      ggplot(aes(x = year, y = lifeExp, color = country)) +
      geom_line() + geom_point()
    
   })
  
}