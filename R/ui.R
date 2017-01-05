shinydashboard::dashboardPage(skin = "blue",
  shinydashboard::dashboardHeader(title = shiny::div(shiny::img(src = "E.png", width = "15%", height = "15%"), "moTector")),
  shinydashboard::dashboardSidebar(
    shiny::div(class = "facebookP", "Input Facebook page ID"),
    shinydashboard::sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    shiny::textInput("text", "Text input:")
  ),
  shinydashboard::dashboardBody(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    shiny::h1("this is from shiny header")
  )
)


