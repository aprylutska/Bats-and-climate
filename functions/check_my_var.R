# Custom function to check dependency of selected growth metric on day across years
# data: data frame with dependent and independent variables
# dependent_var: name of the dependent variable, in ""
# independent_var: name of the independent variable, in ""
# years: name of the variable containing years

check_my_var <- function(data, dependent_var, independent_var, years) {
  
  formula <- formula(paste0(dependent_var, " ~ ", independent_var))
  
  years <- unique(data[[years]])
  
  yearly_stat <- list()
  
  for (i in 1:length(years)) {
    
    # Fit simple linear model
    mod <- lm(formula = formula, data = data[data$Year == years[i],])
    
    # Get response coefficient
    coef <- mod$coefficients[2]
    
    # Get p-value from model summary
    p_val <- summary(mod)$coefficients[,"Pr(>|t|)"][2]
    
    yearly_stat[[i]] <- c(coef, p_val)
    
  }
  
  yearly_stat <- as.data.frame(t(as.data.frame(yearly_stat)))
  rownames(yearly_stat) <- years
  colnames(yearly_stat) <- c("Coefficient", "p_value")
  
  # yearly_stat$sign_level <- ifelse(yearly_stat$p-value > 0.05, 
  #                                  " ", 
  #                                  ifelse(yearly_stat$p-value <= 0.05 & yearly_stat$p-value > 0.01,
  #                                         "*",
  #                                         "***")
  #                                  )
  for (k in 1:nrow(yearly_stat)) {
    
    yearly_stat$sign_level[k] <- if(yearly_stat$p_value[k] > 0.05) {" "} else {"*"}
    
  }
  
  p <- ggplot(data = data, aes_string(x = independent_var, y = dependent_var)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(vars(Year))
  
  return(list(yearly_stat, p))
}