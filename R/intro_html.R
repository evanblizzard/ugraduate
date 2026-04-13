# ./R/intro_html.R

intro_content <- tags$div(
  tags$h1("Welcome to UGraduate!"),
  
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
  h5("Variable Definitions:"),
  tags$table(
    class = "table table-sm",
    tags$thead(tags$tr(tags$th("Variable"), tags$th("Definition"))),
    tags$tbody(
      tags$tr(tags$td("Retention Rate"), tags$td("% of first-time, full-time
                                                 undergrads who re-enroll the 
                                                 following fall.")),
      tags$tr(tags$td("Pell %"), tags$td("% of undergrads receiving Pell 
                                         Grants")),
      tags$tr(tags$td("HHI Score"), tags$td("Herfindahl-Hirschman Index - 
                                            measures program concentration.
                                            Higher = more specialized.")),
      tags$tr(tags$td("Avg Net Price"), tags$td("Average net price for 
                                                students receiving any 
                                                financial aid.")),
      tags$tr(tags$td("Avg Net Price (Pell)"), tags$td("Average net price for
                                                       students in the 0-30k
                                                       income bracket, used
                                                       as a Pell proxy due to 
                                                       data availability.")),
      tags$tr(tags$td("Admission Rate"), tags$td("% of applicants admitted.")),
      tags$tr(tags$td("Euclidean Distance"), tags$td("A measure of structural
                                                     similarity between 
                                                     institutions across
                                                     user-selected variables.
                                                     Lower = more similar."))
    ),
    hr(),
    h5("A note on HHI"),
    p("The Herfindahl-Hirschman Index was originally designed to measure market
      concentration in economics. Here it has been adapted to measure program
      concentration, or how evenly or unevenly a school's enrollment is 
      distributed across academic fields. A high HHI means most students are 
      concentrated in one or two subject areas. A low HHI means enrollment
      is spread across many fields. This is a descriptive tool for understanding
      a school's program structure, NOT it's quality."),
    hr(),
    p(em("Data Source: IPEDS 2019-2020 through 2023-24. Built with R Shiny by
         Evan Blizzard, Natalie Cope, and Myles Nwatu."))
  )
)
