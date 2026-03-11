library(shiny)
library(bslib)
library(shinycssloaders)
library(dplyr)
library(readr)

options(spinner.type = 8, spinner.color = "#2C7FB8")

source(file.path("R", "ui-controls.R"))


ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    "navbar-bg" = "#1F4E79"
  ),
  
  title = "Permutation Test Explorer: WPP 2024 Development Groups",
  
  sidebar = sidebar(
    width = 360,
    open = "always",
    
    h4("Analysis settings"),
    
    selectInput(
      inputId = "indicator",
      label = "Choose a demographic indicator",
      choices = list(
        "Birth rate" = "birth_rate",
        "Life expectancy" = "life_exp",
        "Mortality rate" = "mortality"
      ),
      selected = "birth_rate"
    ),
    
    numericInput(
      inputId = "B",
      label = "Number of permutations (B)",
      value = 2000,
      min = 100,
      step = 100
    ),
    
    numericInput(
      inputId = "seed",
      label = "Random seed",
      value = 1,
      min = 1
    ),
    
    numericInput(
      inputId = "alpha",
      label = "Significance level",
      value = 0.05,
      min = 0.001,
      max = 0.20,
      step = 0.001
    ),
    
    selectInput(
      inputId = "alternative",
      label = "Alternative hypothesis",
      choices = c("two.sided", "greater", "less"),
      selected = "two.sided"
    ),
    
    sliderInput(
      inputId = "bins",
      label = "Histogram bins",
      min = 10,
      max = 80,
      value = 40,
      step = 1
    ),
    
    hr(),
    
    h5("About this app"),
    p("This app compares the distribution of a selected demographic indicator between More developed and Less developed countries using a KS-based Monte Carlo permutation test.")
  ),
  
  layout_column_wrap(
    width = 1/2,
    
    
    value_box(
      title = "Observed KS statistic",
      value = div(
        style = "font-size:22px;",
        textOutput("obs_text")
      ),
      theme = "primary"
    ),
    
    value_box(
      title = "Permutation p-value",
      value = div(
        style = "font-size:22px;",
        textOutput("pval_text")
      ),
      theme = "info"
    ),
    
    value_box(
      title = "Decision",
      value = div(
        style = "font-size:20px;",
        textOutput("decision_text")
      ),
      theme = "success"
    ),
    
    value_box(
      title = "Sample sizes",
      value = div(
        style = "font-size:20px;",
        textOutput("n_text")
      ),
      theme = "secondary"
    )
  ),
  
  navset_card_tab(
    nav_panel(
      "Permutation distribution",
      br(),
      withSpinner(plotOutput("perm_plot", height = "500px")),
      br(),
      verbatimTextOutput("hypothesis_text")
    ),
    
    nav_panel(
      "ECDF comparison",
      br(),
      withSpinner(plotOutput("ecdf_plot", height = "500px"))
    ),
    
    nav_panel(
      "Cleaned data preview",
      br(),
      tableOutput("data_preview"),
      br(),
      verbatimTextOutput("summary_text")
    )
  )
)

server <- function(input, output, session) {
  source("server-main.R", local = TRUE)$value
}

shinyApp(ui = ui, server = server)