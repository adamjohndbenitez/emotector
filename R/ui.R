shinydashboard::dashboardPage(skin = "blue",
  shinydashboard::dashboardHeader(title = 
    shiny::div(
      shiny::img(src = "E.png", width = "15%", height = "15%"), "moTector"
    )
  ),
  shinydashboard::dashboardSidebar(
    shiny::div(class = "facebookP", "Input ", 
      shiny::icon(name = "facebook-f", class = "fa-1x", lib = "font-awesome"), "acebook page ID"
    ),
    shinydashboard::sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Search Settings:", icon = shiny::icon(name = "cogs", class = "fa-1x", lib = "font-awesome"),
        shiny::dateRangeInput(inputId = "dateRangeId", label = "Date Range of Posts"),
        shiny::numericInput(inputId = "numberOfPosts", label = "Number of Posts", value = 50)
      )
    ),
    shiny::uiOutput(outputId = "postListUIId")
  ),
  shinydashboard::dashboardBody(
    shiny::uiOutput(outputId = "viewPostUIId"),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
  )
)


