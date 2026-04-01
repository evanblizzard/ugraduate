## 1. Business Logic Section ------------
# UGraduate App
# Evan Blizzard, Natalie Cope, Myles Nwatu
## Library Packages
library(shiny)
library(bslib)
library(plotly)

data <- load("data/master_data.RData")


## Begin User Interface Section
ui <- page_sidebar(
  title = "AU Retention & Peer Benchmarking",
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

nav_panel("Specialization and Retention",
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
              card_footer("Note: Higher HHI = More Specialized. 
                          Lower HHI = More Diversified/Generalist.")
            )
          )
        )
      )



server <- function(input, output) {
  # reactive filter based on ui selections
  filtered_data <- reactive({
    master_data |> 
      filter(instnm %in% input$peer_select)
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
  
}

shinyApp(ui, server)