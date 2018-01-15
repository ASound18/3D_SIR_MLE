library(shiny)

grid_df <- readRDS("grid_df_SIR2.rds")                                                ## read in raw data frame
grid_df = as.data.frame(apply(grid_df, 2, function(x) as.numeric(as.character(x))))   ## somehow numeric does not behave thus coerce
grid_df$logLik = -grid_df$logLik                                                      ## the original logLik returns the negative of loglikelihood
max_Loglik_G = max(grid_df$logLik, na.rm = TRUE)                                      ## all unique values for delta
values = sort(unique(grid_df$measurement_noise_volatility_values))

shinyServer(function(input, output, session) {
  
  output$plot <- renderPlotly({
    input_delta = input$delta
    if(input_delta == 0) input_delta = 1e-4
    temp_df = grid_df[grid_df$measurement_noise_volatility_values == input_delta,]
    x = unique(temp_df$asset_drift_values)
    y = unique(temp_df$asset_volatility_values)
    z = t(matrix(temp_df$logLik, nrow = length(x)))
    maxLogLik = max(temp_df$logLik, na.rm = TRUE)
    
    plot_ly(x = x, y = y, z = z) %>% 
      add_surface() %>%
      add_trace(data = temp_df, x = temp_df$asset_drift_values[temp_df$logLik==maxLogLik],
                y = temp_df$asset_volatility_values[temp_df$logLik==maxLogLik],
                z = maxLogLik,
                mode = "markers", type = "scatter3d")
  })
  
  ### ~~~~ local optimum (give delta) ~~~~ ###
  
  output$maxPoint_x <- renderText({
    input_delta = input$delta
    if(input_delta == 0) input_delta = 1e-4
    temp_df = grid_df[grid_df$measurement_noise_volatility_values == input_delta,]
    maxLogLik = max(temp_df$logLik, na.rm = TRUE)
    ans = temp_df$asset_drift_values[temp_df$logLik==maxLogLik]
    ans[!is.na(ans)]
  })
  
  output$maxPoint_y <- renderText({
    input_delta = input$delta
    if(input_delta == 0) input_delta = 1e-4
    temp_df = grid_df[grid_df$measurement_noise_volatility_values == input_delta,]
    maxLogLik = max(temp_df$logLik, na.rm = TRUE)
    ans = temp_df$asset_volatility_values[temp_df$logLik==maxLogLik]
    ans[!is.na(ans)]
  })
  
  output$maxPoint_z <- renderText({
    input_delta = input$delta
    if(input_delta == 0) input_delta = 1e-4
    temp_df = grid_df[grid_df$measurement_noise_volatility_values == input_delta,]
    max(temp_df$logLik, na.rm = TRUE)
  })
  
  
  ### ~~~~ global optimum ~~~~ ###
  
  output$maxPointG_x <- renderText({
    ans = grid_df$asset_drift_values[grid_df$logLik==max_Loglik_G]
    ans[!is.na(ans)]
  })
  
  output$maxPointG_y <- renderText({
    ans = grid_df$asset_volatility_values[grid_df$logLik==max_Loglik_G]
    ans[!is.na(ans)]
  })
  
  output$maxPointG_delta <- renderText({
    ans = grid_df$measurement_noise_volatility_values[grid_df$logLik==max_Loglik_G]
    ans[!is.na(ans)]
  })
  
  output$maxPointG_z <- renderText({
    max_Loglik_G
  })
  
  
  ### ~~~~ View by the optimal Delta ~~~~ ###
  
  observeEvent(input$optimalDeltaButton, {
    value = grid_df$measurement_noise_volatility_values[grid_df$logLik==max_Loglik_G]
    value = value[!is.na(value)]
    updateSliderInput(session, "delta", label = "Measurement Noise Volatility (Delta):", min = 0, max = max(values), step = 0.005, value = value)
  })
  
})