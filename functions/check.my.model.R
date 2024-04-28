# Custom function to check model fit and assumptions
# model: model object, fitted either by lm(y ~ x + z) / lm(y ~ x * z) or
#         lmer(y ~ x + w + (1|z)) / lmer(y ~ x * w + (1|z))
# Model must have exactly two independent variables

check.my.model <- function(model, mixed = FALSE) {
  library(performance)
  library(gridExtra)
  library(grid)
  
  if(mixed == FALSE) {
    model$model %>% 
      pivot_longer(cols = 2:3,
                   names_to = "independent_var",
                   values_to = "value") %>% 
      separate(independent_var, into = 'predictor', extra = 'drop', remove = FALSE) %>% 
      rename("response_var" = 1) %>% 
      ggplot(aes(x = value, y = response_var, colour = predictor)) +
      geom_point() +
      geom_smooth(method = "lm") +
      facet_wrap(vars(predictor), scales = "free") +
      labs(title = paste0("R2 adjusted = ", round(model_performance(model)$R2_adjusted, digits = 3),
                          ", RMSE = ", round(model_performance(model)$RMSE, digits = 3)
      )) -> p_fit
    
    p_norm <- plot(check_normality(model))
    
    p_heterosced <- plot(check_heteroscedasticity(model))
    
  } else {
    model@frame %>% 
      pivot_longer(cols = 2:3,
                   names_to = "independent_var",
                   values_to = "value") %>% 
      separate(independent_var, into = 'predictor', extra = 'drop', remove = FALSE) %>% 
      rename("response_var" = 1) %>% 
      ggplot(aes(x = value, y = response_var, colour = predictor)) +
      geom_point() +
      geom_smooth(method = "lm") +
      facet_wrap(vars(predictor), scales = "free") +
      labs(title = paste0("R2 conditional = ", 
                          round(performance::r2_nakagawa(model, tolerance = 1e-1000)[[1]], 
                                digits = 3),
                          ", R2 marginal = ", 
                          round(performance::r2_nakagawa(model, tolerance = 1e-1000)[[2]], 
                                digits = 3)
      )) -> p_fit
    
    p_norm <- plot(check_normality(model))
    
    p_heterosced <- plot(check_heteroscedasticity(model))
  }
  
  #  Define layout for gridExtra
  # https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
  lay <- rbind(c(1,1),
               c(1,1),
               c(2,3))
  
  # Graphical output of model's performance
  gridExtra::grid.arrange(p_fit, p_norm, p_heterosced,
                          nrow = 2, ncol = 2,
                          layout_matrix = lay)
  
  # Text output of model's performance
  return(model_performance(model))

}