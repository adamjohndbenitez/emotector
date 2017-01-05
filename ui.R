shinydashboard::dashboardPage(skin = "blue",
  shinydashboard::dashboardHeader(title = div(img(src = "E.png", width = "15%", height = "15%"), "moTector")),
  shinydashboard::dashboardSidebar(
    div(class = "facebookP", "Input Facebook page ID"),
    shinydashboard::sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    textInput("text", "Text input:")
  ),
  shinydashboard::dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
  )
)


