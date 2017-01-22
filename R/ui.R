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
      shinydashboard::menuItem("Search Page ID Settings:", icon = 
        shiny::icon(name = "cogs", class = "fa-1x", lib = "font-awesome"),
        shiny::dateRangeInput(inputId = "dateRangeId", label = "Date Range of Posts"),
        shiny::numericInput(inputId = "numberOfPosts", label = "Number of Posts", value = 10)
      )
    ),
    shiny::tags$br(),
    shiny::tags$hr(),
    shiny::uiOutput(outputId = "postListUIId"),
    shiny::checkboxInput(inputId = "checkManualPostId", label = "Manual input post: ", value = FALSE, width = '100%'),
    shiny::conditionalPanel(condition = "input.checkManualPostId == true", 
      shiny::textAreaInput(inputId = "manualPostTextAreaId", label = NULL, value = "", width = '100%', height = '200px', placeholder = "Input post..."),
      shiny::actionButton(inputId = "submitManualPostId", label = "Submit Post", icon = 
        shiny::icon(name = "hdd-o", class = "fa-1x", lib = "font-awesome"), width = '100%'
      )
    )
  ),
  shinydashboard::dashboardBody(
    shiny::fluidRow(
      shiny::uiOutput(outputId = "viewPostUIId")
    ),
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(outputId = "joyBoxId", width = 3),
      shinydashboard::infoBoxOutput(outputId = "joyHighestBoxId", width = 3),
      shinydashboard::infoBoxOutput(outputId = "joyHigherBoxId", width = 3),
      shinydashboard::infoBoxOutput(outputId = "joyHighBoxId", width = 3),
      shinydashboard::infoBoxOutput(outputId = "joyNeutralBoxId", width = 3),
      shinydashboard::infoBoxOutput(outputId = "joyLowBoxId", width = 3),
      shinydashboard::infoBoxOutput(outputId = "joyLowestBoxId", width = 3)
    ),
    # shiny::fluidRow(
    #   shinydashboard::valueBoxOutput(outputId = "sadnessBoxId", width = 3),
    #   shinydashboard::infoBoxOutput(outputId = "sadnessHighestBoxId", width = 3),
    #   shinydashboard::infoBoxOutput(outputId = "sadnessHigherBoxId", width = 3),
    #   shinydashboard::infoBoxOutput(outputId = "sadnessHighBoxId", width = 3),
    #   shinydashboard::infoBoxOutput(outputId = "sadnessNeutralBoxId", width = 3),
    #   shinydashboard::infoBoxOutput(outputId = "sadnessLowBoxId", width = 3),
    #   shinydashboard::infoBoxOutput(outputId = "sadnessLowestBoxId", width = 3)
    # ),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
  )
)


