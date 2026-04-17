library(tidyverse)
library(shiny)

data<-read.csv("./Lecture Code/pokemon_stats_2025.csv")

## Base page

ui<-fluidPage()

server<-function(input,output){
  

}

shinyApp(ui,server)


## Adding Text - uses HTML


ui<-fluidPage(h1("Here is some text"))

server<-function(input,output){}

shinyApp(ui,server)



## Using columns to control the layout

ui<-fluidPage(h1("App Title"),
              fluidRow(column(3,h2("This is where the user will select the inputs")),
                       column(9,h2("This is where the output will go"))))

server<-function(input,output){}

shinyApp(ui,server)

## Maximum of 12 columns possible, so you need to make sure the first value in the column argument adds to 12.

## Creating inputs

ui<-fluidPage(h1("App Title"),
              fluidRow(column(3,selectInput("pokemon_type", "Pokemon Type",choices = unique(data$type_1)), # selection box
                              textInput("text_input","Type")), # Text box input
                       column(9,tableOutput("pokemontable"))), # Table output
              h2("Some really interesting text"))

server<-function(input,output){}

shinyApp(ui,server)

## There are a number of different input types, you can also have checkboxes, sliders, numerical inputs.
## You can find more here with the code to create them: https://shiny.posit.co/r/gallery/widgets/widget-gallery/

## Creating outputs

ui<-fluidPage(h1("App Title"),
              fluidRow(column(3,selectInput("pokemon_type", "Pokemon Type",choices = unique(data$type_1)),
                       column(9,tableOutput("pokemontable")))))

server<-function(input,output){
  
  
  output$pokemontable<-renderTable({data %>% filter(type_1==input$pokemon_type)})
    
}

shinyApp(ui,server)


## Creating multiple outputs

ui<-fluidPage(h1("App Title"),
              fluidRow(column(3,selectInput("pokemon_type", "Pokemon Type",choices = unique(data$type_1))),
                       column(9,tableOutput("pokemontable"))), # Table output next to input selection
              plotOutput("plot")) # Plot output under the table spanning the whole width

server<-function(input,output){
  
  output$pokemontable<-renderTable({data %>% filter(type_1==input$pokemon_type)})
  
  
  output$plot<-renderPlot({
    
    data %>% filter(type_1==input$pokemon_type) %>% 
      ggplot(aes(x=name,y=base_experience))+geom_point()
    
  })
  
}

shinyApp(ui,server)


## Delaying the rendering of outputs with buttons


ui<-fluidPage(h1("App Title"),
              fluidRow(column(3,selectInput("pokemon_type", "Pokemon Type",choices = unique(data$type_1)),
                              actionButton("go","Get Data")), ## action button
                       column(9,tableOutput("pokemontable"))),
              plotOutput("plot"))

server<-function(input,output){
  
  Type<-eventReactive(input$go,{Type<-input$pokemon_type})
  
  output$pokemontable<-renderTable({data %>% filter(type_1==Type())})
  
  
  output$plot<-renderPlot({
    
    data %>% filter(type_1==input$pokemon_type) %>% 
      ggplot(aes(x=name,y=base_experience))+geom_point(col="purple",size=4, shape=6)+
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
    
  })
  
}

shinyApp(ui,server)

## The table will not be rendered automatically because it is being controlled by the button, the graph will change
## when the type is changed
