## 1. Business Logic Section ------------
# UGraduate App
# Evan Blizzard, Natalie Cope, Myles Nwatu

## Library Packages
library(shiny)
library(bslib)
library(plotly)
library(dplyr) # Added this to fix the error I was seeing that said 
# could not find function "pull"

data <- load("data/master_data.RData")

# variables used in multiple places
metric_choices <- c(
  "Retention Rate"          = "ret_pres",
  "Pell %"                  = "pell_pct",
  "HHI (Specialization)"    = "hhi_score",
  "Avg Net Price"           = "net_price_avg",
  "Avg Net Price (Pell)"    = "net_price_avg_pell"
)


## Begin User Interface Section
ui <- page_sidebar(
  title = "AU Retention & Peer Benchmarking",
  # Making sidebar global
  sidebar = sidebar(
    checkboxGroupInput("filter_control", "1. Select Sector:",
                       choices = c("Public = 1", 
                                   "Private Nonprofit" = 2,
                                   "Private For-Profit" = 3),
                       selected = c(1,2)),
    uiOutput("state_selector"),
    uiOutput("school_selector"),
    
    hr(),
    
    radioButtons("metric", "Trend metric:",
                 choices = c("Retention"                = "ret_pres",
                             "Pell %"                   = "pell_pct",
                             "HHI (Specialization)" = "hhi_score")),
    
    hr(),
    
    selectInput("year_select", "Scatter year:",
                choices = levels(master_data$academic_year),
                selected = "2023-24"),
    checkboxInput("show_line", "Show trend line", value = TRUE)
  ),
  
  navset_card_underline(

  # TAB 1: Trends
    nav_panel(
      title = "Institutional Trends",
      layout_sidebar(
        sidebar = sidebar(
          radioButtons("metric", "Select Metric:",
                       choices = metric_choices)
        ),
        card(
          card_header("Trend Over Time"),
          plotlyOutput("trend_plot")
        )
      )
    ),
    
    # TAB 2: HHI vs Retention
    nav_panel(
      title = "Explore Relationships",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("scatter_x", "X-axis:",
                      choices = metric_choices,
                      selected = "hhi_score"),
          selectInput("scatter_y", "Y-axis:",
                      choices = metric_choices,
                      selected = "ret_pres"),
          selectInput("year_select", "Academic Year:",
                      choices = levels(master_data$academic_year),
                      selected = "2023-24"),
          checkboxInput("show_line", "Show Trend Line", value = TRUE)
        ),
        card(
          card_header("Relationship Explorer"),
          plotlyOutput("scatter_plot"),
          card_footer("")
        )
      )
    ),


server <- function(input, output) {
  # state dropdown based on sector selection
  output$state_selector <- renderUI({
    states <- master_data |> 
      filter(as.integer(control) %in% input$filter_control) |> 
      pull(stabbr) |> unique() |> sort()
    
    selectInput("filter_state", "2. Select State(s):",
                choices = states, multiple = TRUE, selected = "DC")
  })
  # school dropdown based on sector + state
  output$school_selector <- renderUI({
    req(input$filter_state)
    
    schools <- master_data |> 
      filter(as.integer(control) %in% input$filter_control,
             stabbr %in% input$filter_state) |> 
      pull(instnm) |> unique() |> sort()
    
    selectizeInput("peer_select", "3. Select Peer Institution(s):",
                   choices = schools,
                   selected = schools[1],
                   multiple = TRUE,
                   options = list(placeholder = "Type to search"))
  })
  # all schools in the sector+state filter
  background_data <- reactive({
    req(input$filter_state)
    master_data |> 
      filter(as.integer(control) %in% input$filter_control,
             stabbr %in% input$filter_state)
  })
  # just for the trend line data on tab 1
  filtered_data <- reactive({
    req(input$peer_select)
    master_data |> 
      filter(instnm %in% input$peer_select) |> 
      filter(!is.na(.data[[input$metric]]))
  })
  
  
  # Tab 1 Trend Plot
  output$trend_plot <- renderPlotly({
    p <- ggplot(filtered_data(),
                aes(x = academic_year,
                    y = .data[[input$metric]],
                    group = instnm,
                    color = instnm)) +
      geom_line(size = 1) +
      geom_point() +
      labs(y = "Value", x = "Academic Year", color = "Institution") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Tab 2 scatter explorer
  # two layers, gray background points (all schools) + colored selected schools

  
  # Extra Relationship Tab - Pell Scatter:
  output$pell_scatter <- renderPlotly({
    
    p <- ggplot(pell_filtered(),
                aes(x = pell_pct,
                    y = ret_pres,
                    text = instnm)) +
      
      
      # trying to eventually match colors of the institutional trends tab 
      # color = "#E6550D" is a placeholder
      geom_point(color = "#E6550D", size = 3, alpha = 0.8) + 
      labs(
        x = "Pell Grant Percentage (%)",
        y = "Retention Rate (%)",
        title = "Is Financial Aid Associated with Retention?"
      ) +
      
      theme_minimal()
    
    if (input$show_line_pell) {
      p <- p + geom_smooth(method = "lm",
                           color = "black",
                           linetype = "dashed",
                           se = FALSE)
    }
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)