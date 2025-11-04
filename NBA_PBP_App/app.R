
#Loading Library
library(tidyverse)
library(shiny)
library(DT)

#Relative path to data
data_path <- file.path("data", "nba_scoring_subset_1718.rds")
if (!file.exists(data_path)) {
  stop("Missing data file at: ", normalizePath(data_path, mustWork = FALSE))
}
pbp_condensed <- readRDS(data_path)

#Defining the UI
ui <- fluidPage(
  titlePanel("NBA Play-by-Play Scoring Efficiency (2017/2018) App"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Subset the Data"),
      
      #Categorical Variables
      radioButtons(
        inputId = "quarter_filter",
        label = "Quarter:",
        choiceNames = c("All", "Q1", "Q2", "Q3", "Q4"),
        choiceValues = c("all", "Q1", "Q2", "Q3", "Q4")
      ),
      radioButtons(
        inputId = "outcome_filter",
        label = "Shot Outcome",
        choiceNames = c("All", "Made", "Missed"),
        choiceValues = c("all", "make", "miss")
      ),
      
      #Numeric Variables
      h2("Numeric Variable Filters"),
      selectInput(
        inputId = "number1",
        label = "First numeric variable",
        choices = c("Shot Distance (ft)" = "ShotDist", 
                    "Points Scored" = "Points", 
                    "Home Team Total Points" = "HomeScore", 
                    "Away Team Total Points" = "AwayScore"),
        selected = "ShotDist"
      ),
      uiOutput("slider1"),
      
      selectInput(
        inputId = "number2",
        label = "Second numeric variable:",
        choices = c("Shot Distance (ft)" = "ShotDist", 
                    "Points Scored" = "Points", 
                    "Home Team Total Points" = "HomeScore", 
                    "Away Team Total Points" = "AwayScore"),
        selected = "Points"
      ),
      uiOutput("slider2"),
      
      br(),
      actionButton("apply_filters", "Apply Filters", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel(
          "About",
          h3("What does this app do?"),
          p("Use the sidebar to subset the data (quarters, shot outcomes, and numeric ranges), then click ",
            strong("Apply Filters"), " to update the app."),
          h4("Data Source"),
          p("Play-by-play data from the NBA seasons (2016–2021), condensed to scoring/shot variables from 2017–2018."),
          tags$ul(
            tags$li("Sidebar: Choose categorical filters and two numeric range filters."),
            tags$li("Data Download: View and download the subsetted rows."),
            tags$li("Data Exploration: Quick categorical and numeric summaries/plots of the subset.")
          )
        ),
        
        tabPanel(
          "Data Download",
          h3("Subsetted Data"),
          DT::dataTableOutput("data_table"),
          br(),
          downloadButton("download_csv", "Download subset as CSV")
        ),
        
        tabPanel(
          "Data Exploration",
          fluidRow(
            column(
              width = 3,
              h4("Choose summaries"),
              radioButtons("exp_mode", "Summary Type:",
                           choices = c("Categorical" = "cat", "Numeric" = "num"),
                           selected = "cat"),
              
              #Categorical Variable Options
              conditionalPanel(
                condition = "input.exp_mode == 'cat'",
                selectInput("exp_cat1", "Categorical Variable 1:",
                            choices = c("QuarterLabel","ShotOutcome","HomeTeam","AwayTeam","year"),
                            selected = "QuarterLabel"),
                selectInput("exp_cat2", "Categorical Variable 2 (optional):",
                            choices = c("None","ShotOutcome","QuarterLabel","HomeTeam","AwayTeam","year"),
                            selected = "ShotOutcome")
              ),
              
              #Numeric Variable Options
              conditionalPanel(
                condition = "input.exp_mode == 'num'",
                selectInput("exp_num", "Numeric Variable:",
                            choices = c("ShotDist","Points","HomeScore","AwayScore"),
                            selected = "ShotDist"),
                selectInput("exp_group", "Group by (optional):",
                            choices = c("None","QuarterLabel","ShotOutcome","HomeTeam","AwayTeam","year"),
                            selected = "None"),
                radioButtons("exp_plot_type", "Plot type:",
                             choices = c("Histogram" = "hist", "Boxplot" = "box"),
                             selected = "hist")
              )
            ),
            column(
              width = 8,
              h4("Summary"),
              verbatimTextOutput("summary_text"),
              plotOutput("explore_plot")
            )
          )
        )
      )
    )
  )
)

#Server
server <- function(input, output, session){
  
  #Dynamic Sliders
  output$slider1 <- renderUI({
    x <- pbp_condensed[[input$number1]]
    rang <- range(x, na.rm = TRUE)
    sliderInput("range1", 
                paste("Range for", input$number1),
                min = floor(rang[1]), 
                max = ceiling(rang[2]), 
                value = rang)
  })
  output$slider2 <- renderUI({
    x <- pbp_condensed[[input$number2]]
    rang <- range(x, na.rm = TRUE)
    sliderInput("range2", 
                paste("Range for", input$number2),
                min = floor(rang[1]), 
                max = ceiling(rang[2]), 
                value = rang)
  })
  
  #Filters Logic
  rv <- reactiveValues(data = pbp_condensed)
  
  observeEvent(input$apply_filters, {
    df <- pbp_condensed
    
    if (input$quarter_filter != "all") {
      df <- dplyr::filter(df, QuarterLabel == input$quarter_filter)
    }
    if (input$outcome_filter != "all"){
      df <- dplyr::filter(df, ShotOutcome == input$outcome_filter)
    }
    
    if (!is.null(input$range1)) {
      v1 <- input$number1
      df <- dplyr::filter(
        df,
        !is.na(.data[[v1]]),
        .data[[v1]] >= input$range1[1],
        .data[[v1]] <= input$range1[2]
      )
    }
    
    if (!is.null(input$range2)) {
      v2 <- input$number2
      df <- dplyr::filter(
        df,
        !is.na(.data[[v2]]),
        .data[[v2]] >= input$range2[1],
        .data[[v2]] <= input$range2[2]
      )
    }
    
    rv$data <- df
  })
  
  #Data Download Tab
  output$data_table <- DT::renderDataTable({
    validate(need(nrow(rv$data) >0 , "No rows match the current subset."))
    cols <- c("year","QuarterLabel","HomeTeam","AwayTeam","ShotType","ShotOutcome","ShotDist","Points", "HomeScore","AwayScore")
    keep <- intersect(cols, names(rv$data))
    out <- rv$data[keep]
    if ("year" %in% names(out)) out$year <- as.character(out$year)
    out
  }, options = list(pageLength = 15))
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("nba_subset_", Sys.Date(), ".csv"),
    content = function(file) {
      req(nrow(rv$data) > 0)
      write.csv(rv$data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  #Data Exploration Tab
  
  #Helper Function
  getcol <- function(df, nm) {
    if (nm %in% names(df)) {
      df[[nm]]
    } else { NULL }
  }
  
  #Summaries
  output$summary_text <- renderPrint({
    validate(need(nrow(rv$data) > 0, "No rows match the current subset."))
    
    if (input$exp_mode == "cat") {
      c1 <- input$exp_cat1
      c2 <- input$exp_cat2
      x <- getcol(rv$data, c1)
      validate(need(!is.null(x), paste("Column", c1, "not found.")))
      
      if (c2 == "None") {
        print(table(as.character(x), useNA = "no"))
      } else{
        y <- getcol(rv$data, c2)
        validate(need(!is.null(y), paste("Column", c2, "not found.")))
        print(addmargins(table(as.character(x), as.character(y), useNA = "no")))
      }
      
      #Numeric Summaries
    } else { 
      num <- input$exp_num
      grp <- input$exp_group
      v <- getcol(rv$data, num)
      validate(need(is.numeric(v), paste(num, "must be numeric.")))
      
      if (grp == "None") {
        print(summary(v))
      } else { 
        g <- getcol(rv$data, grp)
        validate(need(!is.null(g), paste("Grouping column", grp, "not found.")))
        g <- as.character(g)
        s <- tapply(v, g, summary)
        print(s)
      }
    }
  })
  #Data Exploration Plots
  output$explore_plot <- renderPlot({
    validate(need(nrow(rv$data) > 0, "No rows match the current subset."))
    
    if (input$exp_mode == "cat") {
      c1 <- input$exp_cat1
      c2 <- input$exp_cat2
      df <- rv$data
      validate(need(c1 %in% names(df), paste("Column", c1, "not found.")))
      df[[c1]] <- as.factor(df[[c1]])
      
      if (c2 == "None") { 
        ggplot(df, aes(x = .data[[c1]])) + geom_bar() + labs(x = c1, y = "Count")
      } else {
        validate(need(c2 %in% names(df), paste("Column", c2, "not found.")))
        df[[c2]] <- as.factor(df[[c2]])
        ggplot(df, aes(x = .data[[c1]], fill = .data[[c2]])) + geom_bar(position = "dodge") + 
          labs(x = c1, y = "Count", fill = c2)
      }
    } else {
      #Numeric Variable Plots
      num <-input$exp_num
      grp <- input$exp_group
      df <- rv$data
      validate(need(num %in% names(df), paste("Column", num, "not found.")))
      validate(need(is.numeric(df[[num]]), paste(num, "must be numeric.")))
      
      if (grp == "None") {
        if (input$exp_plot_type == "hist") {
          ggplot(df, aes(x = .data[[num]])) + geom_histogram(bins = 25) + labs(x = num, y = "Frequency")
        } else {
          ggplot(df, aes(y = .data[[num]])) + geom_boxplot() + labs(y = num)
        }
      } else {
        validate(need(grp %in% names(df), paste("Grouping column", grp, "not found.")))
        df[[grp]] <- as.factor(df[[grp]])
        if (input$exp_plot_type == "hist") {
          ggplot(df, aes(x = .data[[num]], fill = .data[[grp]])) + geom_histogram(bins = 25) + 
            labs(x = num, y = "Frequency", fill = grp)
        } else {
          ggplot(df, aes(x = .data[[grp]], y = .data[[num]])) + geom_boxplot() + labs(x = grp, y = num)
        }
      }
      
    }
  })
}




#Running the App
shinyApp(ui = ui, server = server)