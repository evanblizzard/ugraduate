# ./R/intro_html.R

intro_content <- tags$div(
  tagsh1("Welcome to UGraduate!"),
  
  tags$p(
    "This app uses data from the Integrated Postsecondary Education Data 
    System (IPEDS), a federally mandated reporting system published by the 
    National Center for Education Statistics (NCES). All data is publicly
    available."
  ),
  tags$p(
    "The app is designed for institutional administrators and researchers (as
    well as statistically interested students) who want to compare student
    retention outcomes across peer institutions as well as view relationships
    between variables like Herfindahl-Hirschman Index specialization scores,
    average prices, and admissions data."
  ),
  hr(),
  h5("How to use the app:"),
  tags$ol(
    tags$li("Use the sidebar to filter by sector and state, then select schools,
            you want to analyze."),
    tags$li("Use the Institutional Trends tab to track metrics over time."),
    tags$li("Use the Explore Relationships tab to scatter any two variables
            against each other."),
    tags$li("Use the Find My Peers tab to identify structurally similar
            institutions via Euclidean distance."),
    tags$li("Use the Retention Gap tab to compare overall vs. Pell student
            retention.")
  ),
  hr(),
  h5("Variable Definitions:")
)