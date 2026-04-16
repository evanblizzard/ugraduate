---
editor_options: 
  markdown: 
    wrap: 72
---

# UGraduate: Retention, Peer Benchmarking, and Exploratory Analysis

An interactive R Shiny application for exploring undergraduate student
retention outcomes across U.S. postsecondary institutions using publicly
available federal data from the Integrated Postsecondary Education Data
System.

**Built by:** Evan Blizzard, Natalie Cope, and Myles Nwatu American
University - DATA-413/613, Spring 2026

------------------------------------------------------------------------

## Purpose

UGraduate is designed for institutional administrators and researchers
who want to compare student retention outcomes across peer institutions
and investigate relationships between retention, cost, financial aid
profiles, and program specialization. The app supports:

-   Tracking institutional metrics over time
-   Exploring relationships between variables (e.g, admissions vs.
    retention)
-   Identifying structurally similar peer institutions via Euclidean
    distance
-   Analyzing retention gaps between overall and Pell Grant recipient
    students
-   Modeling predictors of retention rates using multivariate linear
    regression

## App Layout

| Tab | Description |
|------------------------------------|------------------------------------|
| **About** | App context, variable definitions, HHI explanation, and workflow guide |
| **Institutional Trends** | Line charts tracking metrics over time for selected schools |
| **Explore Relationships** | Scatterplot explorer for any two variables with optional trend line |
| **Peer Benchmarking** | Euclidean distance-based peer institution discover |
| **Retention Gap** | Grouped bar chart comparing overall vs. Pell retention |
| **Retention Modeling** | Multivariate linear regression with coefficient plots and diagnostics |
| **Data** | Filterable data with CSV download |

------------------------------------------------------------------------

## Getting Started

### Prerequisites

-   R (\>= 4.30)
-   RStudio (Recommended)

### Installation

1.  Clone this repository using this
    [URL](https://github.com/26S-DATA-x13-03/gp-emn-job-descriptions.git)

2.  Restore package environment if necessary using `{renv}`:

    ``` r
    install.packages("renv")
    renv::restore()
    ```

3.  Run the app using RStudio interface or:

    ```{r}
    shiny::runApp()
    ```

## Data

### Source
