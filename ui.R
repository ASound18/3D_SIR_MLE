library(shiny)
library(plotly)

grid_df <- readRDS("grid_df_SIR15.rds")                                               ## read in raw data frame
grid_df = as.data.frame(apply(grid_df, 2, function(x) as.numeric(as.character(x))))  ## somehow numeric does not behave thus coerce
values = sort(unique(grid_df$measurement_noise_volatility_values))                   ## all unique values for delta


shinyUI(fluidPage(
  
  titlePanel("Particle Filtering-based Maximum Likelihood Estimation"),
  
  tabsetPanel(
    tabPanel("View By Single Layer",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("delta", "Measurement Noise Volatility (Delta):", min = 0, max = max(values), step = 0.002, value = 0, animate=TRUE),
                 br(),br(),
                 fluidRow(
                   column(12, h4(strong("Global Optimum"))),
                   column(12, h5(strong("Optimal Asset Drift:"))),
                   column(12, textOutput("maxPointG_x")),
                   column(12, h5(strong("Optimal Asset Volatility:"))),
                   column(12, textOutput("maxPointG_y")),
                   column(12, h5(strong("Optimal Delta:"))),
                   column(6, textOutput("maxPointG_delta")),
                   column(6, actionButton("optimalDeltaButton", "View by this optimal Delta", icon = icon("eye"))),
                   column(12, h5(strong("Optimal Log-Likelihood:"))),
                   column(12, textOutput("maxPointG_z"))
                 )
               ),
               
               mainPanel(
                 plotlyOutput("plot"),
                 fluidRow(
                   column(12, h4(strong("Local Optimum"))),
                   column(12, h5(strong("Optimal Asset Drift:"))),
                   column(12, textOutput("maxPoint_x")),
                   column(12, h5(strong("Optimal Asset Volatility:"))),
                   column(12, textOutput("maxPoint_y")),
                   column(12, h5(strong("Optimal Log-Likelihood:"))),
                   column(12, textOutput("maxPoint_z"))
                 )
                 
               )
             )
            ),
    
    tabPanel("View All Layers", 
             fluidRow(column(8, offset=3,  plotlyOutput("plot2")))
             )
  )
  
  
  
))