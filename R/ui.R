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
      shiny::uiOutput(outputId = "downloadCSV")
    ),
    shiny::tags$hr(),
    shiny::div(class= "submitPostBtn", " ",
         shiny::actionButton(inputId = "submitManualPostId", label = "Analyze Post", icon = 
              shiny::icon(name = "hdd-o", class = "fa-1x", lib = "font-awesome"), width = "70%"
         )  
    ),
    shiny::tags$hr(),
    
    shiny::checkboxInput(inputId = "checkManualPostId", label = "Analyze / Override Posts: ", value = FALSE, width = "100%"),
    shiny::conditionalPanel(condition = "input.checkManualPostId == true", 
      shiny::textAreaInput(inputId = "manualPostTextAreaId", label = NULL, value = "", width = "100%", height = "200px", placeholder = "You can override post from facebook API, by removing page id and input here."),
      shiny::div(class= "submitPostBtn", " ",
         shiny::actionButton(inputId = "submitManualPostId", label = "Analyze Post", icon = 
            shiny::icon(name = "hdd-o", class = "fa-1x", lib = "font-awesome"), width = "70%"
         )  
      )
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
      column(width = 12, 
        fixedRow(
          column(width = 6,
             shiny::fluidRow(
               shiny::uiOutput(outputId = "showSummaryWeightedEmotion")
             )
          ),
          column(width = 6,
            shiny::fluidRow(
              shiny::uiOutput(outputId = "showDegreeOfEmotion")
            )
          )
        )
      )
    )
  ),
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)


