library(shiny)

grid_df <- readRDS("grid_df_SIR.rds")
grid_df$measurement_noise_volatility_values = as.numeric(as.character(grid_df$measurement_noise_volatility_values))
grid_df$logLik = -grid_df$logLik

shinyServer(function(input, output, session) {
  
  output$plot <- renderPlotly({
    input_delta = input$delta
    if(input_delta == 0) input_delta = 1e-4
    temp_df = grid_df[grid_df$measurement_noise_volatility_values == input_delta,]
    x = unique(temp_df$asset_drift_values)
    y = unique(temp_df$asset_volatility_values)
    z = matrix(temp_df$logLik, nrow = length(y))
    maxLogLik = max(temp_df$logLik)
    
    plot_ly(x = x, y = y, z = z) %>% 
      add_surface() %>%
      add_trace(data = temp_df, x = temp_df$asset_drift_values[temp_df$logLik==maxLogLik],
                y = temp_df$asset_volatility_values[temp_df$logLik==maxLogLik],
                z = maxLogLik,
                mode = "markers", type = "scatter3d")
  })
  
  output$maxPoint_x <- renderText({
    input_delta = input$delta
    if(input_delta == 0) input_delta = 1e-4
    temp_df = grid_df[grid_df$measurement_noise_volatility_values == input_delta,]
    maxLogLik = max(temp_df$logLik)
    temp_df$asset_drift_values[temp_df$logLik==maxLogLik]
  })
  
  output$maxPoint_y <- renderText({
    input_delta = input$delta
    if(input_delta == 0) input_delta = 1e-4
    temp_df = grid_df[grid_df$measurement_noise_volatility_values == input_delta,]
    maxLogLik = max(temp_df$logLik)
    temp_df$asset_volatility_values[temp_df$logLik==maxLogLik]
  })
  
  output$maxPoint_z <- renderText({
    input_delta = input$delta
    if(input_delta == 0) input_delta = 1e-4
    temp_df = grid_df[grid_df$measurement_noise_volatility_values == input_delta,]
    max(temp_df$logLik)
  })
  
  
})