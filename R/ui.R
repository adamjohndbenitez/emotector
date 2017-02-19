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
        shiny::dateRangeInput(inputId = "dateRangeId", label = 
          shiny::tagList(
            shiny::icon(name = "calendar", class = "fa-1x", lib = "font-awesome"),
            "Date Range of Posts"
          )
        ),
        shiny::numericInput(inputId = "numberOfPosts", label = 
          shiny::tagList(
            shiny::icon(name = "comment", class = "fa-1x", lib = "font-awesome"),
            "Numbe of Posts"
          ), value = 5
        ),
        shiny::numericInput(inputId = "numberOfComments", label = 
          shiny::tagList(
            shiny::icon(name = "comments", class = "fa-1x", lib = "font-awesome"),
            "Number of comments"
          ), value = 5
        )
      )
    ),
    shiny::tags$br(),
    shiny::tags$hr(),
    shiny::div(class = "facebookP", " ",
      shiny::uiOutput(outputId = "downloadCSVUIId")
    ),
    shiny::tags$hr(),
    shiny::div(class= "submitPostBtn", " ",
        shiny::actionButton(inputId = "submitAnalyzePostId", label = "Analyze Emotions", icon = 
          shiny::icon(name = "fa-binoculars", class = "fa-1x", lib = "font-awesome"), width = "70%")
      ),
    shiny::tags$hr(),
    shiny::checkboxInput(inputId = "checkManualPostId", label = "Testbox", value = FALSE, width = "100%"),
    shiny::conditionalPanel(condition = "input.checkManualPostId == true", 
      shiny::textAreaInput(inputId = "manualPostTextAreaId", label = NULL, value = "", width = "100%", height = "200px", placeholder = "You can manually test a post, by providing input in this testbox. However, if you provide input in the search input, it will override manual test post")
    )
  ),
  shinydashboard::dashboardBody(
    # shiny::div(
    #   shiny::img(src = "insideout.gif", width = "100%", height = "10%")
    # ),
    shiny::fluidRow(
      shiny::uiOutput(outputId = "viewPostUIId")
    ),
    shiny::fluidRow(
      shiny::uiOutput(outputId = "viewCommentsUIId")
    ),
    
    shiny::fluidRow(
      shiny::uiOutput(outputId = "showEmotionsBoxes")
    ),
    
    shiny::fixedRow(
      shiny::column(width = 12, 
        shiny::fixedRow(
          shiny::column(width = 6,
            shiny::fluidRow(
              shiny::uiOutput(outputId = "showSummaryWeightedEmotion")
            )
          ),
          shiny::column(width = 6,
            shiny::fluidRow(
              shiny::uiOutput(outputId = "showDegreeOfEmotion")
            )
          )
        )
      )
    ),
    shiny::fixedRow(
      shiny::column(width = 12, 
        shiny::fluidRow(
          shiny::uiOutput(outputId = "showWordCloud")
        )
      )
    )
  ),
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)


