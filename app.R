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
        selectInput("peer_select", "Select Peer Institution:",
                    choices = unique(master_data$instnm),
                    selected = "American University",
                    multiple = TRUE),
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
  # reactive filter based on ui selections for Tab 1
  filtered_data <- reactive({
    master_data |> 
      filter(instnm %in% input$peer_select)
  })
  
  # reactive filter for Tab 2 (filters by year)
  hhi_filtered <- reactive({
    master_data |> 
      filter(academic_year == input$year_select)
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
      theme_minmal() +
      theme(legend.position = "none") # hiding legnd
    
    # logic for adding regression line
    if (input$show_line) {
      p <- p + geom_smooth(method = "lm", color = "black", linetype = "dashed", 
                           se = FALSE)
    }
    
    # using tooltip = "text" to show institution names on hover
    ggplotly(p, tooltip = "text")
  })
  
}

shinyApp(ui, server)