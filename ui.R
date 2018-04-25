
library(shiny)


shinyUI(navbarPage("Exploratory Pairs",
    tabPanel("Paired Graphs",
    sidebarPanel(
        selectInput(inputId= "dataset", label= "Data Set", choices=c("mtcars", "swiss")),
        selectInput(inputId= "x", label= "X Variable", choices=""),
        selectInput(inputId= "y", label= "Y Variable", choices=""),
        selectInput(inputId= "z", label= "Color", choices=""),
        h4("Model"),
        textOutput("modout"),
        h4("Correlation"),
        textOutput("corout")
    ),
    mainPanel(
        plotOutput("plot1" , brush = brushOpts(id = "brush1"))
    )
    ),
    tabPanel("About",
             mainPanel(includeMarkdown("about.md")                )
             )
    
))