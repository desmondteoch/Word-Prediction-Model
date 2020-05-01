suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
suppressWarnings(library(RColorBrewer))

shinyUI(navbarPage("Data Science Capstone - Word Prediction",
                   
            # main tab
            tabPanel("Predict the Next Word",
                    HTML("<strong>Author: Desmond Teo</strong>"),
                    br(),
                    HTML("<strong>Date: 01/05/2020</strong>"),
                    br()
                    ),
                    
            # sidebar
            sidebarLayout(
                    sidebarPanel(
                        helpText("Enter some words below to receive a next word prediction"),
                        textInput("inputString", "Enter words here", value = "")
                        ),
                    mainPanel(
                        h2("Predicted Next Word"),
                        verbatimTextOutput("prediction"),
                        strong("Sentence Input:"),
                        tags$style(type='text/css', 
                                   '#text1 {background-color: rgba(177,175,182,1); 
                                   color: blue;}'), 
                        textOutput('text1'),
                        br(),
                        strong("Note:"),
                        tags$style(type='text/css', 
                                   '#text2 {background-color: rgba(177,175,182,1); 
                                   color: black;}'),
                        textOutput('text2')
                    )
            )
        )
    )
