
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(shiny)
require("Rfacebook")
library(shinyjs)

dashboardPage( skin = "blue",
  dashboardHeader(
    title=div(img(src="E.png", width = "15%", height = "15%"), "moTector")
  ),
  dashboardSidebar(
    div(class = "facebookP", "Input Facebook page ID"),
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    textInput("text", "Text input:")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
  )
)


