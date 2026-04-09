## 1. Business Logic Section ------------
# UGraduate App
# Evan Blizzard, Natalie Cope, Myles Nwatu

## Library Packages
library(shiny)
library(bslib)
library(plotly)
library(dplyr) # Added this to fix the error I was seeing that said 
# could not find function "pull"
library(ggplot2)

data <- load("data/master_data.RData")

# variables used in multiple places
metric_choices <- c(
  "Retention Rate"          = "ret_pres",
  "Pell %"                  = "pell_pct",
  "HHI (Specialization)"    = "hhi_score",
  "Avg Net Price"           = "net_price_avg",
  "Avg Net Price (Pell)"    = "net_price_avg_pell"
)

# functions
### Euclidean Distance Function
euclidean <- function(school, pool, vars, n = 10) {
  # pull columns to measure distance on
  mat <- pool |> select(unitid, instnm, all_of(vars))
  
  # drop rows with NA in selected variables
  mat <- mat |> filter(if_all(all_of(vars), ~is.na(.)))
  
  # using z score normalization
  mat_scaled <- mat |> 
    mutate(across(all_of(vars), scale))
  
  # get the selected school's NORMALIZED values as a vector
  target <- mat_scaled |> 
    filter(instnm == selected_school) |> 
    select(all_of(vars)) |> 
    as.numeric()
  
  # calculate distance from target to every other school
  
  mat_scaled |> 
    filter(instnm != selected_school) |> 
    rowwise() |> 
    mutate(    # this is just euclidean distance formula
               # sqrt of the sum of the squared differences between selected school each of the other schools
      distance = sqrt(sum((c_across(all_of(vars)) - target)^2))
    ) |> 
    ungroup()
  slice_min(distance, n = n) # to compare with requested amount of schools
  select(unitid, instnm, distance)
}


## Begin User Interface Section
ui <- page_sidebar(
  title = "AU Retention & Peer Benchmarking",
  # Making sidebar global
  sidebar = sidebar(
    checkboxGroupInput("filter_control", "1. Select Sector:",
                       choices = c("Public" = 1, 
                                   "Private Nonprofit" = 2,
                                   "Private For-Profit" = 3),
                       selected = c(1,2)),
    uiOutput("state_selector"),
    uiOutput("school_selector")
    
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
    )
  )
)


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
  output$scatter_plot <- renderPlotly({
    req(input$scatter_x, input$scatter_y, input$year_select)
    
    x_var <- input$scatter_x
    y_var <- input$scatter_y
    
    # full background (all schools meeting filter conditions)
    bg <- background_data() |> 
      filter(academic_year == input$year_select) |> 
      filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
    
    # highlighted (only selected schools)
    selected <- bg |> 
      filter(instnm %in% input$peer_select)
    
    x_label <- names(metric_choices)[metric_choices == x_var]
    y_label <- names(metric_choices)[metric_choices == y_var]
    
    p <- ggplot() +
      # first layer: all schools
      geom_point(data = bg,
                 aes(x = .data[[x_var]], y = .data[[y_var]], text = instnm),
                 color = "gray70", size = 2, alpha = 0.5) +
    # layer 2: all selected schools colored and labeled
      geom_point(data = selected,
                 aes(x = .data[[x_var]], y = .data[[y_var]],
                     color = instnm, text = instnm),
                 size = 4) +
        labs(x = x_label, y = y_label, color = "Selected Schools") +
        theme_minimal()
    
    if (input$show_line) {
      p <- p + geom_smooth(data = bg,
                           aes(x = .data[[x_var]], y = .data[[y_var]]),
                           method = "lm",
                           color = "black",
                           alpha = 0.4,
                           linewidth = 0.5,
                           se = FALSE)
    }
    ggplotly(p, tooltip = "text")
  })

}

shinyApp(ui, server)