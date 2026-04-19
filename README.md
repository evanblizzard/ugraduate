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
|----|----|
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

This app uses data from the **Integrated Postsecondary Education Data
System (IPEDS),** administered by the National Center for Education
Statistics (NCES).

-   Coverage: Academic years from 2019-20 through 2023-24

-   URL: <https://nces.ed.gov/ipeds/>

### Access

IPEDS data is publicly available and free to download. No account or API
key is required. Raw data files and the cleaning script that produces
`data/master_data.Rdata` are in `data_raw/`.

### Licenses and Restrictions

IPEDS is a product of the U.S. federal government and is in the public
domain. There are no restrictions on its use for educational or research
purposes.

## Responsible Data Science Considerations

### Privacy

IPEDS reports institution-level aggregated data only. No individual
student records are included. Users should exercise caution when
interpreting results for small or specialized institutions where Pell
recipient cohorts may be very small.

## Fairness and Interpretation

Retention gaps between overall and Pell students reflect systemic
structural inequities and should not be interpreted as institutional
failures without broader context.

The HHI score is adapted from economics and is a descriptive measure of
program concentration. It is **not a rating of institutional quality**.

The retention model identifies statistical association, not causal
relationships. Coefficients reflect cross-sectional patterns and should
not be used to make causal inferences about any specific institution's
policies.

## Team and Contact

| Name | Role | Github |
|----|----|----|
| Evan Blizzard | Data ingestion, visualizations, UI, statistical modeling | <https://github.com/evanblizzard> |
| Natalie Cope |  | <https://github.com/EverClearie> |
| Myles Nwatu |  | <https://github.com/a-nwatu> |

## License

This project is licensed under the **[MIT / GPL-3.0]** license. See
`LICENSE` for details.

Key third-party package licenses: `shiny` (GPL-3), `tidyverse` (MIT),
`plotly` (MIT), `bslib` (MIT), `DT` (GPL-3), `broom` (MIT),
`shinyWidgets` (GPL-3).
