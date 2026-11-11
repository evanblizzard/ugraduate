## 1. Business Logic Section ------------
# UGraduate App
# Evan Blizzard, Natalie Cope, Adaobi Nwatu

## Library Packages
library(shiny)
library(bslib)
library(plotly)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(broom)

load("data/master_data.RData")
# adding this to get rid of schools that don't have values for all of the
# variables
master_data <- master_data |> 
  filter(!if_all(any_of(c("ret_pres", "pell_pct", "hhi_score", "net_price_avg",
                          "net_price_avg_pell", "admit_rate", "admissions")), 
                 is.na))
source("./R/intro_html.R")

# institution names aren't unique (~25 duplicates / year) and some schools change names over time
  # so everything is keyed on unitid
  # display label uses each school's most recent name + city and statewith unitid appended
  # in case that is still ambiguous
school labels <- master_data |>
  arrange(unitid, desc(academic_year)) |>
  distinct(unitid, .keep_all=TRUE) |>
  transmute(unitid, label=paste0(instnm, " (", city, ", ", stabbr, ")")) |>
  group_by(label) |>
  mutate(label = if (n() > 1) paste0(label, " [", unitid, "]") else label) |>
  ungroup()



# variables used in multiple places
metric_choices <- c(
  "Retention Rate"          = "ret_pres",
  "Pell %"                  = "pell_pct",
  "HHI (Specialization)"    = "hhi_score",
  "Avg Net Price"           = "net_price_avg",
  "Avg Net Price (Pell)"    = "net_price_avg_pell",
  "Admissions Rate (%)"     = "admit_rate",
  "Total Admissions"       = "admissions"
)

# functions
### Euclidean Distance Function
euclidean <- function(school, pool, vars, n = 10) {
  # pull columns to measure distance on
  mat <- pool |> select(unitid, instnm, all_of(vars))
  
  # drop rows with NA in selected variables
  mat <- mat |> filter(if_all(all_of(vars), ~!is.na(.))) # using a lambda 
          # . is a placeholder meaning whatever column its currently looking at
          # is not NA? keep that row.
  # using z score normalization
  mat_scaled <- mat |> 
    # scale() subtracts column mean and divides by standard deviation
    # ensures every variable is centered at 0 with spread of 1
    mutate(across(all_of(vars), scale))
  
  # get the selected school's NORMALIZED values as a vector
  target <- mat_scaled |> 
    filter(instnm == school) |> 
    select(all_of(vars)) |> 
    as.numeric()
  
  # calculate distance from target to every other school
  
  mat_scaled |> 
    # make sure selected school isn't in the pool
    filter(instnm != school) |> 
    # rowwise() so each row is treated as its own group
    # makes mutate run one row at a time
    # c_across(all_of(vars)) grabs current' rows variables for all 
    # variables as a vector
    rowwise() |> 
    mutate(    # this is just euclidean distance formula
               # sqrt of the sum of the squared differences between selected school each of the other schools
      distance = sqrt(sum((c_across(all_of(vars)) - target)^2))
    ) |> 
    # have to ungroup after using rowwise to return to normal data frame
    ungroup() |> 
  slice_min(distance, n = n) |> # to compare with requested amount of schools
  select(unitid, instnm, distance)
}


## Begin User Interface Section
ui <- page_sidebar(
  title = "UGraduate", #added theme for polishing
  theme = bs_theme(bootswatch = "morph"),
  # Making sidebar global
  sidebar = sidebar(
    # school selection
    checkboxGroupInput("filter_control", "1. Select Sector:",
                       choices = c("Public" = 1, 
                                   "Private Nonprofit" = 2,
                                   "Private For-Profit" = 3),
                       selected = c(1,2)),
    uiOutput("state_selector"),
    uiOutput("school_selector"),
    
    hr(),
    # inputs for the euclidean distance peer calculation
    pickerInput("peer_vars", "Peer matching variables:",
                choices = metric_choices, 
                selected = c("admit_rate", "net_price_avg", 
                             "pell_pct", "hhi_score"),
                multiple = TRUE,
                options = list(title = "Select Variables...")),
    actionButton("find_peers_btn", "Find My Peers", class = "btn-primary w-100")
    
    
  ),
  
  navset_card_underline(
    
    nav_panel("About", intro_content),

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
    nav_panel("Peer Benchmarking",
              layout_sidebar(
                sidebar = sidebar(
                  selectInput("peer_year", "Academic Year:",
                              choices = levels(master_data$academic_year),
                              selected = "2023-24")
                ),
                card(
                  card_header("Most Similar Institutions"),
                  DT::dataTableOutput("peer_table")
                )
              )
            ),

  # retention modeling panel with an additional diagnostics tab for checking
  # assumptions of linear regression
  nav_panel("Retention Modeling",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("model_year", "Academic Year:",
                      choices = levels(master_data$academic_year),
                      selected = "2023-24"),
          hr(),
          p("Model predicts retention rate from admission rate,
             net price, Pell %, and HHI score across all schools
             in the selected year.",
            style = "font-size: 0.85rem; color: gray;")
        ),
        navset_tab(

          nav_panel("Results",
            layout_column_wrap(
              width = 1,
              card(
                card_header("Predictor Effects"),
                plotlyOutput("coef_plot"),
                card_footer(uiOutput("model_stats"))
              ),
              card(
                card_header("Predicted vs. Actual Retention"),
                plotlyOutput("pred_actual_plot"),
                card_footer("Points above the dashed line retain better than
                             predicted. Points below retain worse.
                             Selected schools are highlighted.")
              )
            )
          ),

          nav_panel("Model Diagnostics",
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Residuals vs Fitted"),
                plotlyOutput("residual_plot"),
                card_footer("Points should be randomly scattered around zero.
                             A pattern suggests the linear assumption may not hold.")
              ),
              card(
                card_header("Normal Q-Q Plot"),
                plotlyOutput("qq_plot"),
                card_footer("Points should follow the dashed line closely.
                             Deviations suggest residuals may not be normally
                             distributed.")
                )
              )
            )
          )
        )
      ),
  nav_panel("Data",
            card(
              card_header("Filtered Dataset"),
              layout_column_wrap(
                width = 1, 
                downloadButton("download_data", "Download CSV"),
                DT::dataTableOutput("data_table")
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
  

  
  # retention model reactive
  retention_model <- reactive({
    req(input$model_year)
    
    model_data <- master_data |> 
      filter(academic_year == input$model_year) |> 
      select(ret_pres, admit_rate, net_price_avg, pell_pct, hhi_score) |> 
      # drop any school missing any of the variables
      filter(if_all(everything(), ~!is.na(.)))
    
    validate(
      need(nrow(model_data) > 30,
           "Not enough data to fit a model for this year.")
    )
    lm(ret_pres ~ admit_rate + net_price_avg + pell_pct + hhi_score,
       data = model_data)
  })
  
  
  # Tab 1 Trend Plot
  output$trend_plot <- renderPlotly({
    validate(
      need(nrow(filtered_data()) > 0,
           "No data available. Please select at least one school.")
    )
    p <- ggplot(filtered_data(),
                aes(x = academic_year,
                    y = .data[[input$metric]],
                    group = instnm,
                    color = instnm)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(y = "Value", x = "Academic Year", color = "Institution") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Tab 2 scatter explorer
  # two layers, gray background points (all schools) + colored selected schools
  output$scatter_plot <- renderPlotly({
    validate(
      need(nrow(filtered_data()) > 0,
           "No data available. Please select at least one school.")
    )
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
  
  # Tab 3 Peer Benchmarking
  peers <- eventReactive(input$find_peers_btn, {
    req(input$peer_select, input$peer_vars)
    
    # require at least 2 variables for euclidean distance calc
    validate(
      need(length(input$peer_vars) >= 2, "Please select at least 2 variables.")
    )
    
    # only use selected year for matching
    pool <- master_data |> filter(academic_year == input$peer_year)
    
    # run function for each selected school
    map_dfr(input$peer_select, function(school) {
      result <- euclidean(school, pool, input$peer_vars, n = 10)
      result$reference_school <- school
      result
    })
  })
  
  output$peer_table <- DT::renderDataTable({
    peers() |> 
      select(
        `Your School` = reference_school,
        `Peer School` = instnm,
        `Distance`    = distance
      ) |> 
      mutate(Distance = round(Distance, 3))
  },
  options = list(pageLength = 10),
  rownames = FALSE)
  
  
  
  # coefficient plot for linear model
  output$coef_plot <- renderPlotly({
    model <- retention_model()
    
    # tidy() to extract coefficients into a clean df
    # conf.int = TRUE to add 95% conf int columns
    coefs <- tidy(model, conf.int = TRUE) |> 
      filter(term != "(Intercept)") |> #cutting intercept bc not meaningful
      mutate(
        # making readable labels instead of column names
        term = recode(term,
                      "admit_rate" = "Admission Rate",
                      "net_price_avg" = "Avg Net Price",
                      "pell_pct" = "Pell %",
                      "hhi_score" = "HHI Score"),
        # flagging significant predictors so i can color them
        significant = p.value < 0.05
      )
    
    p <- ggplot(coefs,
                aes(x = estimate, y = reorder(term, estimate),
                    color = significant,
                    text = paste0(term,
                                  "\nEffect: ", round(estimate, 3),
                                  "\np-value: ", round(p.value, 3)))) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(x = "Effect on Retention Rate (percentage points)",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text")
    
  })
  
  # card below coefficient plot with model fit statistics
  output$model_stats <- renderUI({
    stats <- glance(retention_model())
    
    tagList(
      p(strong("R²: "), round(stats$r.squared, 3),
        " - the model explains ",
        round(stats$r.squared * 100, 1),
        "% of the variation in retention rates."),
      p(strong("Adjusted R²: "), round(stats$adj.r.squared, 3)),
      p(strong("Observations: "), stats$nobs)
    )
  })
  
  # predicted vs actual values scatterplot
  output$pred_actual_plot <- renderPlotly({
    req(input$peer_select)
    
    model <- retention_model()
    
    model_data <- master_data |> 
      filter(academic_year == input$model_year) |> 
      select(instnm, ret_pres, admit_rate, net_price_avg, pell_pct, hhi_score) |> 
      filter(if_all(where(is.numeric), ~!is.na(.)))
    # using augment to add the .fitted and .resid columns to the original data
    # so i can plot them
    augmented <- augment(model, newdata = model_data)
    
    
    # splitting into background schools and selected schools
    bg <- augmented
    selected <- augmented |> filter(instnm %in% input$peer_select)
    
    p <- ggplot() +
      # 45 degree line for reference. points on this line = perfect fit
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "gray50") +
      # all schools in gray
      geom_point(data = bg,
                 aes(x = .fitted, y = ret_pres, text = instnm),
                 color = "gray70", size = 2, alpha = 0.5) +
      # color selected schools
      geom_point(data = selected,
                 aes(x = .fitted, y = ret_pres, 
                     color = instnm, text = paste0(
                       instnm,
                   "\nActual: ", round(ret_pres, 1), "%",
                   "\nPredicted: ", round(.fitted, 1), "%",
                   "\nResidual: ", round(ret_pres - .fitted, 1), " pts")),
                 size = 4) +
      labs(x = "Predicted Retention (%)",
           y = "Actual Retention (%)",
           color = NULL) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # residual plot
  output$residual_plot <- renderPlotly({
    model <- retention_model()
    augmented <- augment(model)
    
    p <- ggplot(augmented, aes(x = .fitted, y = .resid)) +
      geom_point(color = "gray60", alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Fitted Values", y = "Residuals",
           title = "Residuals vs Fitted") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # qq plot
  output$qq_plot <- renderPlotly({
    model <- retention_model()
    augmented <- augment(model) |> 
      # using qqnorm with plot.it = F to get just the theoretical quantiles
      # as a list
      # just math, no plot
      # using .std.resid for the computation of the quantiles
      mutate(theoretical = qqnorm(.std.resid, plot.it = FALSE)$x)
    p <- ggplot(augmented, aes(x = theoretical, y = .std.resid)) +
      geom_point(color = "gray60", alpha = 0.5) +
      geom_abline(linetype = "dashed", color = "red") +
      labs(x = "Theoretical Quantiles", y = "Standardized Residuals",
           title = "Normal Q-Q Plot") + 
      theme_minimal()
    
    ggplotly(p)
  })
  
  # data table output
  output$data_table <- DT::renderDataTable({
    background_data() |> 
      filter(academic_year == input$year_select) |> 
      select(instnm, stabbr, control, ret_pres, pell_pct, hhi_score,
             net_price_avg, admit_rate)
  },
  rownames = FALSE)
  
  # download output
  output$download_data <- downloadHandler(
    # using function to dynamically name the csv file with the year the user
    # has selected
    filename = function() paste0("ipeds_data_", input$year_select, ".csv"),
    # using function to write the file to csv to a variable called content
    content = function(file) {
      write.csv(background_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)