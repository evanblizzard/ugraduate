## 1. Business Logic Section ------------
# UGraduate App
# Evan Blizzard, Natalie Cope, Myles Nwatu
## Library Packages
library(shiny)
library(bslib)
library(plotly)

data <- load("../clean/master_data.RData")


## Begin User Interface Section
ui <- page_sidebar(
  title = "AU Retention & Peer Benchmarking",
  sidebar = sidebar(
    selectInput("peer_select", "Select Peer Institution:",
                choices = unique(master_data$instnm),
                selected = "American University",
                multiple = TRUE),
    selectInput("metric", "Select Metric:",
                choices = c("Retention" = "ret_pres",
                            "Pell %" = "pell_pct",
                            "HHI (Specialization)" = "hhi_score"))
  ),
  card(
    card_header("Trend Over Time"),
    plotlyOutput("trend_plot")
  )
)

server <- function(input, output) {
  
}

shinyApp(ui, server)