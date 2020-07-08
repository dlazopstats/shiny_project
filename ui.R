library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinycssloaders)

shinyUI(dashboardPage(
    dashboardHeader(title = "Coursera"),
    dashboardSidebar(
        fluidPage(
            titlePanel(title= "Predictive Models")
        ),
        hr(),
        numericInput("porc", "Create Data Partition", 0.7,min=0.6,max=0.9),
        h2(),
        useShinyalert(),
        actionButton("runProfileAlgorithm", "Run Models",
                     icon=icon("fas fa-project-diagram")),
        uiOutput("spinner")
    ),
    dashboardBody(
        fluidRow(
            tabBox(
                title = tagList(shiny::icon("gear"), "Models"),
                tabPanel("Linear Regression",
                         fluidRow(
                             box(title = "Predicted vs Real",width=8,
                                 plotOutput("plot_l1")),
                             box(title = "Variable Importance",width=8,
                                 plotOutput("plot_l2"))
                         )
                ),
                tabPanel("Lasso Regression",
                         fluidRow(
                             box(title = "Predicted vs Real",width=8,
                                 plotOutput("plot_la1")),
                             box(title = "Variable Importance",width=8,
                                 plotOutput("plot_la2"))
                         )
                ),
                tabPanel("GAM Regression",
                         fluidRow(
                             box(title = "Predicted vs Real",width=8,
                                 plotOutput("plot_ga1")),
                             box(title = "Variable Importance",width=8,
                                 plotOutput("plot_ga2"))
                         )
                ),
                tabPanel("GBM Regression",
                         fluidRow(
                             box(title = "Predicted vs Real",width=8,
                                 plotOutput("plot_gb1")),
                             box(title = "Variable Importance",width=8,
                                 plotOutput("plot_gb2"))
                         )
                ),
                tabPanel("Model Comparison",
                         fluidRow(
                             box(title = "MAE",width=8,
                                 plotOutput("comp1")),
                             box(title = "R Squared",width=8,
                                 plotOutput("comp2"))
                         )
                ),
                width = NULL)

        )
    )))
