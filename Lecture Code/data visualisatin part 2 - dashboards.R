library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)

data<-read.csv("./Lecture Code/pokemon_stats_2025.csv")


## Basic dashboard set up

ui <- dashboardPage(
  
  dashboardHeader(title="Exciting Dashboard"),
  
  dashboardSidebar(
    sidebarUserPanel("Menu Title"),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("First Tab", tabName = "tab1"),
      menuItem("Second Tab", tabName = "tab2"),
      menuItem("Third Tab", tabName = "tab3"
      )
    )
  ),
  
  
  dashboardBody(
    # setup any tab pages you want, the number of tabs can be increased.
    tabItems(
      tabItem("tab1", "First Tab"),
      tabItem("tab2", "Second Tab"),
      tabItem("tab3", "Third Tab")
    )
  )
)


#### server code

server <- function(input, output, session) {
  
  
  
  # tab 1 UI and output ----------------------------------------
  
  
  
  # tab 2 UI and output ----------------------------------------
  
  
  
  
  # overview tab UI and output ----------------------------------------
  
  
  
  
  
  
}

shinyApp(ui,server)


## Adding the inputs and outputs


ui <- dashboardPage(
  
  dashboardHeader(title="Exciting Dashboard"),
  
  dashboardSidebar(
    sidebarUserPanel("Menu Title"),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("First Tab", tabName = "tab1"),
      menuItem("Second Tab", tabName = "tab2"),
      menuItem("Third Tab", tabName = "tab3"
      )
    )
  ),
  
  
  dashboardBody(
    # setup any tab pages you want, the number of tabs can be increased.
    tabItems(
      tabItem("tab1", "First Tab",
              selectInput("pokemon_type", "Pokemon Type",choices = unique(data$type_1)),
              plotOutput("tab1_plot")),
      tabItem("tab2", "Second Tab",
              fluidRow(column(3,selectInput("pokemon_type1", "Pokemon Type",choices = unique(data$type_1))),
                       column(9,tableOutput("pokemontable")))),
      tabItem("tab3", "Third Tab")
    )
  )
)


#### server code

server <- function(input, output, session) {
  
  
  
  # tab 1 UI and output ----------------------------------------
  
  output$tab1_plot<-renderPlot({
    data %>% filter(type_1==input$pokemon_type) %>% 
      ggplot(aes(x=name,y=base_experience))+geom_point()
  })
  
  # tab 2 UI and output ----------------------------------------
  
  output$pokemontable<-renderTable({data %>% filter(type_1==input$pokemon_type1)})
  
  # overview tab UI and output ----------------------------------------
  
  
  
  
  
}

shinyApp(ui,server)


## Plotly + extras

ui <- dashboardPage(
  
  dashboardHeader(title="Exciting Dashboard"),
  
  dashboardSidebar(
    sidebarUserPanel("Menu Title"),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("First Tab", tabName = "tab1",icon = icon("hippo")),
      menuItem("Second Tab", tabName = "tab2",icon = icon("otter")),
      menuItem("Third Tab", tabName = "tab3", icon = icon("locust")
      )
    )
  ),
  
  
  dashboardBody(
    # setup any tab pages you want, the number of tabs can be increased.
    tabItems(
      tabItem("tab1", "First Tab",
              selectInput("pokemon_type", "Pokemon Type",choices = unique(data$type_1)),
              plotOutput("tab1_plot")),
      tabItem("tab2", "Second Tab",
              fluidRow(column(3,selectInput("pokemon_type", "Pokemon Type",choices = unique(data$type_1))),
                       column(9,tableOutput("pokemontable")))),
      tabItem("tab3", "Third Tab",
              fluidRow(column(3,selectInput("pokemon_stat1", "First Stat",choices = c("Base Experience"="base_experience",
                                                                                      "Height"="height",
                                                                                      "Weight"="weight",
                                                                                      "HP"="hp",
                                                                                      "Attack"="attack",
                                                                                      "Defense"="defense",
                                                                                      "Speed"="speed",
                                                                                      "Special Attack"="special_attack",
                                                                                      "Special Defence"="special_defence")),
                              selectInput("pokemon_stat2", "Second Stat",choices = c("Base Experience"="base_experience",
                                                                                     "Height"="height",
                                                                                     "Weight"="weight",
                                                                                     "HP"="hp",
                                                                                     "Attack"="attack",
                                                                                     "Defense"="defense",
                                                                                     "Speed"="speed",
                                                                                     "Special Attack"="special_attack",
                                                                                     "Special Defence"="special_defence"))),
                       column(9,plotlyOutput("tab3_plot"))
              )
      )
    )))


#### server code

server <- function(input, output, session) {
  
  
  
  # tab 1 UI and output ----------------------------------------
  
  output$tab1_plot<-renderPlot({
    data %>% filter(type_1==input$pokemon_type) %>% 
      ggplot(aes(x=name,y=base_experience))+geom_point()
  })
  
  # tab 2 UI and output ----------------------------------------
  
  output$pokemontable<-renderTable({data %>% filter(type_1==input$pokemon_type1)})
  
  # overview tab UI and output ----------------------------------------
  
  
  output$tab3_plot<-renderPlotly({
    
    ggplot<-ggplot(data,aes(x=get(input$pokemon_stat1),y=get(input$pokemon_stat2),colour = type_1))+
      geom_point(aes(text=name))+
      theme(legend.position = "none")+xlab(input$pokemon_stat1)+ylab(input$pokemon_stat2)
    
    ggplotly(ggplot,tooltip = c("text"))
    
  })
  
  
}

shinyApp(ui,server)
