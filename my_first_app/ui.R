fluidPage(
  
  titlePanel("test"),
  # Copy the chunk below to make a group of checkboxes
  checkboxGroupInput("out1",
                     label = h3("select country"), 
                     choices = list("Canada" = "Canada", "Rwanda" = "Rwanda", "Cambodia" = "Cambodia", "Mexico" = "Mexico"),
                     selected = "Canada"),
  
  mainPanel(
    plotOutput("out1"))
  
)install.packages("shinydashboard")