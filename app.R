## 1. Business Logic Section ------------
# UGraduate App
# Evan Blizzard, Natalie Cope, Myles Nwatu
## Library Packages
library(shiny)
library(bslib)
library(plotly)

data <- load("data/master_data.RData")


## Begin User Interface Section
ui <- page_navbar(
  title = "AU Retention & Peer Benchmarking",
  
  # TAB 1: Trends
  nav_panel(
    title = "Institutional Trends",
    layout_sidebar(
      sidebar = sidebar(
        # step 1: sector filtering
        checkboxGroupInput("filter_control", "1. Select Sector:",
                    choices = c("Public" = 1, 
                                "Private Nonprofit" = 2, 
                                "Private For-Profit" = 3),
                                selected = c(1,2)), 
        # step 2: state filtering dependent on sector
        uiOutput("state_selector"),
        # step 3: institution filtering dependent on sector
        uiOutput("school_selector"),
        
        hr(), # line break
                    
                    
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
  ), # close tab 1
  # Tab 2 Specialization and Retention Regression
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
  ) # Close Tab 2
) # Close page_navbar
      



server <- function(input, output) {
  # generate state list based on sector choice
  output$state_selector <- renderUI({
    states <- master_data |> 
      filter(as.integer(control) %in% input$filter_control) |> 
      pull(stabbr) |> unique() |> sort()
    
    selectInput("filter_state", "2. Select State(s):",
                choices = states, multiple = TRUE, selected = "DC")
    
  })
  
  # generate school list based on sector AND state
  output$school_selector <- renderUI({
    req(input$filter_state) # only show if state is picked
    
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
  # reactive filter for Tab 2 (filters by year and peer selection)
  hhi_filtered <- reactive({
    req(input$peer_select, input$year_select)
    
    master_data |> 
      filter(academic_year == input$year_select,
             instnm %in% input$peer_select) |> 
      # NEW: Ensure both variables for the scatter plot are present
      filter(!is.na(hhi_score), !is.na(ret_pres))
  })
  
  # rendering trend_plot
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
    # making ggplot interactive with plotly
    ggplotly(p)
  })
  
  # rendering hhi_scatter plot (Tab 2)
  output$hhi_scatter <- renderPlotly({
    # building base plot
    p <- ggplot(hhi_filtered(),
                aes(x = hhi_score,
                    y = ret_pres,
                    text = instnm)) + 
      geom_point(aes(color = instnm), size = 3) +
      labs(x = "Program Specialization (HHI Score)",
           y = "Retention Rate (%)") +
      theme_minimal() +
      theme(legend.position = "none") # hiding legend
    
    # logic for adding regression line
    if (input$show_line) {
      p <- p + geom_smooth(method = "lm", 
                           color = "black", 
                           linetype = "dashed", 
                           se = FALSE)
    }
    
    # using tooltip = "text" to show institution names on hover
    ggplotly(p, tooltip = "text")
  })
  
}

shinyApp(ui, server)