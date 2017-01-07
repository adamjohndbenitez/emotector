shinydashboard::dashboardPage(skin = "blue",
  shinydashboard::dashboardHeader(title = 
    shiny::div(
      shiny::img(src = "E.png", width = "15%", height = "15%"), "moTector"
    )
  ),
  shinydashboard::dashboardSidebar(
    shiny::div(class = "facebookP", "Input Facebook page ID"),
    shinydashboard::sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Search Settings:", icon = icon("cogs"),
        shiny::dateRangeInput(inputId = "dateRangeId", label = "Date Range of Posts"),
        shiny::numericInput(inputId = "numberOfPosts", label = "Number of Posts", value = 50)
      )
    ),
    shiny::selectInput(inputId = "postListId", label = "Select Post", choices = "")
  ),
  shinydashboard::dashboardBody(
    shinydashboard::box(title = "Post", width = 12, solidHeader = TRUE, status = "primary", 
        shiny::textOutput(outputId = "viewPostId")
    ),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
  )
)


