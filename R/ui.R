shinyjs::useShinyjs()
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
    shiny::uiOutput(outputId = "postListUIId"),
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Search Page ID Settings:", icon = 
        shiny::icon(name = "cogs", class = "fa-1x", lib = "font-awesome"),
        shiny::dateRangeInput(inputId = "dateRangeId", label = "Date Range of Posts"),
        shiny::numericInput(inputId = "numberOfPosts", label = "Number of Posts", value = 5),
        shiny::numericInput(inputId = "numberOfComments", label = "Number of Comments", value = 5)
      )
    ),
    shiny::tags$br(),
    shiny::tags$hr(),
    shiny::div(class = "facebookP", " ",
      shiny::uiOutput(outputId = "downloadCSV")
    ),
    shiny::tags$hr(),
    shiny::checkboxInput(inputId = "checkManualPostId", label = "Manual input post: ", value = FALSE, width = "100%"),
    shiny::conditionalPanel(condition = "input.checkManualPostId == true", 
      shiny::textAreaInput(inputId = "manualPostTextAreaId", label = NULL, value = "", width = "100%", height = "200px", placeholder = "Input post..."),
      shiny::div(class= "submitPostBtn", " ",
         shiny::actionButton(inputId = "submitManualPostId", label = "Submit Post", icon = 
            shiny::icon(name = "hdd-o", class = "fa-1x", lib = "font-awesome"), width = "70%"
         )  
      )
    )
  ),
  shinydashboard::dashboardBody(
    shiny::fluidRow(
      shiny::uiOutput(outputId = "viewPostUIId")
    ),
    shiny::fluidRow(
      shiny::uiOutput(outputId = "viewCommentsUIId")
    ),
    shiny::fluidRow(
    shinydashboard::tabBox(id = "tabBoxId", selected = "Joy", title = "Emotions", width = 12, height = NULL, side = "left",
      shiny::tabPanel(title = "Joy", value = "Joy", icon = shiny::icon(name = "smile-o", class = "fa-1x", lib = "font-awesome"),
        shiny::fluidRow(
          shinydashboard::valueBoxOutput(outputId = "joyBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "joyHighestBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "joyHigherBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "joyHighBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "joyNeutralBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "joyLowBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "joyLowestBoxId", width = 3)
        )
      ),
      shiny::tabPanel(title = "Sadness", value = "Sadness", icon = shiny::icon(name = "frown-o", class = "fa-1x", lib = "font-awesome"),
        shiny::fluidRow(
          shinydashboard::valueBoxOutput(outputId = "sadnessBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "sadnessHighestBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "sadnessHigherBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "sadnessHighBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "sadnessNeutralBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "sadnessLowBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "sadnessLowestBoxId", width = 3)
        )
      ),
      shiny::tabPanel(title = "Anger", value = "Anger", icon = shiny::icon(name = "hand-rock-o", class = "fa-1x", lib = "font-awesome"),
        shiny::fluidRow(
          shinydashboard::valueBoxOutput(outputId = "angerBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "angerHighestBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "angerHigherBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "angerHighBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "angerNeutralBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "angerLowBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "angerLowestBoxId", width = 3)
        )
      ),
      shiny::tabPanel(title = "Disgust", value = "Disgust", icon = shiny::icon(name = "thumbs-o-down", class = "fa-1x", lib = "font-awesome"),
        shiny::fluidRow(
          shinydashboard::valueBoxOutput(outputId = "disgustBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "disgustHighestBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "disgustHigherBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "disgustHighBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "disgustNeutralBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "disgustLowBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "disgustLowestBoxId", width = 3)
        )
      ),
      shiny::tabPanel(title = "Fear", value = "Fear", icon = shiny::icon(name = "heartbeat", class = "fa-1x", lib = "font-awesome"),
        shiny::fluidRow(
          shinydashboard::valueBoxOutput(outputId = "fearBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "fearHighestBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "fearHigherBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "fearHighBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "fearNeutralBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "fearLowBoxId", width = 3),
          shinydashboard::infoBoxOutput(outputId = "fearLowestBoxId", width = 3)
        )
      )
    )
    ),
    shiny::fluidRow(
      shinydashboard::box(title = "Summary of Weighted Emotions", width = 12, solidHeader = FALSE, status = "primary", background = NULL,
        shiny::plotOutput("plot")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(title = "Degree of Emotions (Count-based Stacked Barplot)", width = 12, solidHeader = FALSE, status = "primary", background = NULL,
        shiny::plotOutput("plot1")
      )
    )
  ),
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)


