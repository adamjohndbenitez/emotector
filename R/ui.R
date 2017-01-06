shinydashboard::dashboardPage(skin = "blue",
  shinydashboard::dashboardHeader(title = 
    shiny::div(
      shiny::img(src = "E.png", width = "15%", height = "15%"), "moTector"
    )
  ),
  shinydashboard::dashboardSidebar(
    shiny::div(class = "facebookP", "Input Facebook page ID"),
    shinydashboard::sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search...")
  ),
  shinydashboard::dashboardBody(
    shinydashboard::box(title = "Message Generated", width = 4, solidHeader = TRUE, status = "primary", 
      shiny::textOutput(outputId = "nText")
    ),
    shinydashboard::box(
      title = "SAMPLE", width = 4, solidHeader = TRUE, status = "primary", 
        shiny::textOutput(outputId = "sample")
    ),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
  )
)


