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


## Begin User Interface Section
ui <- page_navbar(
  title = "AU Retention & Peer Benchmarking",
  
  # TAB 1: Trends
  nav_panel(
    title = "Institutional Trends",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput("filter_control", "1. Select Sector:",
                           choices = c("Public" = 1, 
                                       "Private Nonprofit" = 2, 
                                       "Private For-Profit" = 3),
                           selected = c(1,2)), 
        uiOutput("state_selector"),
        uiOutput("school_selector"),
        
        hr(),
        
        radioButtons("metric", "Select Metric:",
                     choices = c("Retention" = "ret_pres",
                                 "Pell %" = "pell_pct",
                                 "HHI (Specialization)" = "hhi_score"))
      ),
      card(
        card_header("Trend Over Time"),
        plotlyOutput("trend_plot")
      )
    )
  ),
  
  # TAB 2: HHI vs Retention
  nav_panel(
    title = "Specialization and Retention",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("year_select", "Select Academic Year:",
                    choices = levels(master_data$academic_year),
                    selected = "2023-24"),
        checkboxInput("show_line", "Show Trend Line", value = TRUE)
      ),
      card(
        card_header("Does Specialization Drive Retention?"),
        plotlyOutput("hhi_scatter"),
        card_footer("Note: Higher HHI = More Specialized. Lower HHI = More Diversified/Generalist.")
      )
    )
  ),
  
  # ➕ NEW TAB 3: Pell vs Retention
  nav_panel(
    title = "Pell and Retention",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("year_select_pell", "Select Academic Year:",
                    choices = levels(master_data$academic_year),
                    selected = "2023-24"),
        checkboxInput("show_line_pell", "Show Trend Line", value = TRUE)
      ),
      card(
        card_header("Is Financial Aid Associated with Retention?"),
        plotlyOutput("pell_scatter"),
        card_footer("Note: Pell % is used as a proxy for financial aid / income diversity.")
      )
    )
  )
)


server <- function(input, output) {
  
  output$state_selector <- renderUI({
    states <- master_data |> 
      filter(as.integer(control) %in% input$filter_control) |> 
      pull(stabbr) |> unique() |> sort()
    
    selectInput("filter_state", "2. Select State(s):",
                choices = states, multiple = TRUE, selected = "DC")
  })
  
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
  
  filtered_data <- reactive({
    req(input$peer_select)
    master_data |> 
      filter(instnm %in% input$peer_select) |> 
      filter(!is.na(.data[[input$metric]]))
  })
  
  hhi_filtered <- reactive({
    req(input$peer_select, input$year_select)
    
    master_data |> 
      filter(academic_year == input$year_select,
             instnm %in% input$peer_select) |> 
      filter(!is.na(hhi_score), !is.na(ret_pres))
  })
  
  # ➕ NEW REACTIVE FOR PELL
  pell_filtered <- reactive({
    req(input$peer_select, input$year_select_pell)
    
    master_data |> 
      filter(academic_year == input$year_select_pell,
             instnm %in% input$peer_select) |> 
      filter(!is.na(pell_pct), !is.na(ret_pres))
  })
  
  # Trend Plot
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
  
  # HHI Scatter
  output$hhi_scatter <- renderPlotly({
    p <- ggplot(hhi_filtered(),
                aes(x = hhi_score,
                    y = ret_pres,
                    text = instnm)) + 
      geom_point(aes(color = instnm), size = 3) +
      labs(x = "Program Specialization (HHI Score)",
           y = "Retention Rate (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    if (input$show_line) {
      p <- p + geom_smooth(method = "lm",
                           color = "black",
                           linetype = "dashed",
                           se = FALSE)
    }
    
    ggplotly(p, tooltip = "text")
  })
  
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