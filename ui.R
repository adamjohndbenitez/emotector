#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# 

library(shiny)
require("Rfacebook")
library(shinyjs)


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
  tags$head(tags$script(src = "message-handler.js")),

  # Application title
  titlePanel(title=div(img(src="E.png", width = "5%", height = "5%"), "moTector")),
  #tags$img(src = "line.png", width = "100%", height = "10px"),
  tags$hr(),
 
    sidebarLayout(
      sidebarPanel(
        p("Click generate to fetch Facebook status."),
        img(src="facebook.png", height = 50, width = 50),
        textInput("n", "Input Facebook Page:"),
        br(),
        actionButton("generate", "Generate"),
        actionButton("reset", "Clear"),
        br(), br(),
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')
      ),
      mainPanel(
        tableOutput('contents'),
        p("Searched Word: "),
        verbatimTextOutput("facebookPage")
      )
    )
    
    

    
    
    
    
    


))
