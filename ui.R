library(shiny)

grid_df <- readRDS("grid_df_SIR.rds")
grid_df$measurement_noise_volatility_values = as.numeric(as.character(grid_df$measurement_noise_volatility_values))
values = sort(unique(grid_df$measurement_noise_volatility_values))


shinyUI(fluidPage(
  
  titlePanel("Particle Filtering-based Maximum Likelihood Estimation"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("delta", "Measurement Noise Volatility (Delta):", min = 0, max = max(values), step = 1e-3, value = 0, animate=TRUE),
      br(),br()
      
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
  
))