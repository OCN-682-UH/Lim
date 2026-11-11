### Intro to Shiny Apps
### Created by: Wei Shen Lim
### Created on: 2026-11-10
##########################################################################

### Load Libraries
library(shiny)
library(tidyverse)
library(bslib)


############################### App Template ##################################
# User interface dictates appearance of the app, this write HTML
ui <- fluidPage() 

# Server contains rendering expressions that create the objects to display
server <- function(input, output) {} 

# shinyApp create a Shiny app object with these components
shinyApp(ui = ui, server = server)


############################# First Example ###################################
# Add an element to your app using fluidPage()
ui <- fluidPage('Hello World')
server <- function(input, output) {}
shinyApp(ui = ui, server = server)


########################## Slider Input Example ###############################
# Add input elements to your app using fluidPage()
# Syntax for all inputs is <type>Input()
ui <- fluidPage(
 sliderInput(inputId = "num",              #ID name for the input
            label = "Choose a number",     #label above the input
            value = 25, min = 1, max = 100 #values for the slider
            ) 
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

########################## Plot Output Example ################################
# Add output elements to your app using fluidPage()
# Syntax for all outputs is <type>Output()
# This code creates the space for a new plot, but nothing happens yet
# This is visualized in the order you coded (e.g., slider first then plot)
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100
              ),
  plotOutput("hist") #creates space for a plot called hist
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

########################## Plot Output Render Example ##########################
# Add output render elements to your app in server using function()
# Syntax for all renders is render<Type>()
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100
  ),
  plotOutput("hist")
)

server <- function(input, output) {
  
  #save objects to display to output$
  output$hist <- renderPlot({            #function to render plots
    data <- tibble(x = rnorm(input$num)) #number of points set by the slider
    
    ggplot(data, aes(x = x)) +
      geom_histogram() #make a histogram
  })
}

shinyApp(ui = ui, server = server)


############################ Text Input Example ###############################
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100
              ),
  textInput(inputId = "title",                            #new Id is title
            label = "Write a title",                      #label above input
            value = "Histogram of Random Normal Values"), #starting title
  plotOutput("hist")
)

server <- function(input, output) {
  
  output$hist <- renderPlot({
    data <- tibble(x = rnorm(input$num))
    
    ggplot(data, aes(x=x)) +
      geom_histogram() +
      labs(title = input$title) #add a new title
  })
}

shinyApp(ui = ui, server = server)


########################## Verbatim Text Output Example ########################
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100
  ),
  textInput(inputId = "title",
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  plotOutput("hist"),
  verbatimTextOutput("stats") #create a space for stats
)

server <- function(input, output) {
  
  output$hist <- renderPlot({
    data <- tibble(x = rnorm(input$num))
    
    ggplot(data, aes(x=x)) +
      geom_histogram() +
      labs(title = input$title) #add a new title
  })
  
  output$stats <- renderPrint({ #function to render verbatim texts
    summary(rnorm(input$num))   #calculate summary stats based on the numbers
  })
}

shinyApp(ui = ui, server = server)


####################### Building a reactive object ############################
# Reactive object so the same dataframe is being used throughout all the code
# Call a reactive object like a function: data()
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100
  ),
  textInput(inputId = "title",
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  plotOutput("hist"),
  verbatimTextOutput("stats") #create a space for stats
)

server <- function(input, output) {
  
  data <- reactive({ #making data a reactive object to called into plot output
    tibble(x = rnorm(input$num))
  })
  
  output$hist <- renderPlot({
    ggplot(data(), aes(x=x)) + #must be called like a function: data()
      geom_histogram() +
      labs(title = input$title)
  })
  
  output$stats <- renderPrint({
    summary(data())            #must be called like a function: data()
  })
}

shinyApp(ui = ui, server = server)


########################## Example with Real Data #############################
stars <- read_csv("Data/stars.csv")
glimpse(stars)

# selectInput() creates a dropdown menu
ui <- fluidPage(
  selectInput(inputId = "min_stars",     #ID name for the input
              label = "Show sites with at least this many sea stars:",
              choices = c(1, 5, 10, 20), #threshold options
              selected = 5               #the threshold that shows first
              ),
  plotOutput("star_map") #creates space for the map
)

# filter() to keep only sites that meet the threshold 
# This is how you use selectInput() to pick a minimum count of sea stars
# Each point is a site, sized by the number of sea stars counted there
server <- function(input, output) {
  
  kept_sites <- reactive({
    stars |> 
      
      #selectInput returns choices as STRINGS, so convert back to numeric!
      filter(star_no >= as.numeric(input$min_stars)) #sites above threshold
  })
  
  output$star_map <- renderPlot({
    ggplot(kept_sites(), aes(x = long, y = lat)) +
      geom_point(aes(size = star_no), color = "red") + #points sized by count
      labs(size = "Sea stars counted")
  })
}

shinyApp(ui = ui, server = server)


#################### Layout & Theming with {bslib} ############################
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "cerulean"), # pick a Bootswatch theme
  sidebar = sidebar(                         # everything in the sidebar
    sliderInput(inputId = "num",
                label = "Choose a number",
                value = 25, min = 1, max = 100),
    textInput(inputId = "title",
              label = "Write a title",
              value = "Histogram of Random Normal Values")
  ),
  card(                                     # main content in cards
    card_header("Histogram"),
    plotOutput("hist")
  ),
  card(
    card_header("Summary statistics"),
    verbatimTextOutput("stats")
  )
)

server <- function(input, output) {
  
  data <- reactive({ #making data a reactive object to called into plot output
    tibble(x = rnorm(input$num))
  })
  
  output$hist <- renderPlot({
    ggplot(data(), aes(x=x)) + #must be called like a function: data()
      geom_histogram() +
      labs(title = input$title)
  })
  
  output$stats <- renderPrint({
    summary(data())            #must be called like a function: data()
  })
}

shinyApp(ui = ui, server = server)
