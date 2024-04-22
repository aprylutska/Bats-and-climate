# Custom function to bulk model fitting. Simple linear models with or without interaction
# are used.
# Function returns a series of candidate models (list) and their names (vector), combined 
# in a single list of two.

# Arguments:

# data: data frame with the response variable in a first column and independent variables
#   candidates afterwards. Must have two different kind of variables, each in some number
#   of variants, but neccessarily in equal number. For example, 50 candidates for independent_var1
#   and 50 candidates for independent_var2.

# response_var: name of the response variable, e.g. "Ra"

# interaction: sign of interaction between independent variables, either "+" (no
#   interaction) or "*" (with interaction)


bulk.model.fit <- function(data, response_var, interaction, prefix) {
  
  # Create an empty list to store the models
  models_list <- list()
  
  # Calculate the number of columns for each independent variable
  num_independent_vars <- (ncol(data) - 1) / 2
  
  # Loop through each combination of independent variables
  for (i in 1:num_independent_vars) {
    # Extract the names of the independent variables
    # Add 1 to skip the first column (response variable)
    independent_var1 <- colnames(data)[i + 1]
    # Add num_independent_vars to move to the second set of independent variables
    independent_var2 <- colnames(data)[i + 1 + num_independent_vars]
    
    # Create the formula for the linear model
    formula <- as.formula(paste(response_var, "~", independent_var1, 
                                interaction, independent_var2))
    
    # Fit the linear model
    model <- lm(formula, data = data)
    
    # Add the model to the list
    # Set rules for naming, depending on either there is interaction between 
    # independent variables or not

    if(interaction == "+"){
      model_name <- paste0(prefix, "_", independent_var1, "_", independent_var2)
      } else {
        model_name <- paste0(prefix, "_", independent_var1, "*", independent_var2)
      }

    models_list[[model_name]] <- model
  }
  
  # Access the models using the combination of independent variable names as keys
  # For example, to access the model for the combination of the first 
  # independent variables:
  # models_list[["Tmean_1_Prcp_1"]]
  
  
  # Create an empty vector to store the model names
  model.names <- c()
  
  # Calculate the number of columns for each independent variable
  num_independent_vars <- (ncol(data) - 1) / 2
  
  # Loop through each combination of independent variables
  for (i in 1:num_independent_vars) {
    # Extract the names of the independent variables
    # Add 1 to skip the first column (response variable)
    independent_var1 <- colnames(data)[i + 1]
    # Add num_independent_vars to move to the second set of independent variables
    independent_var2 <- colnames(data)[i + 1 + num_independent_vars]
    
    # Construct the model name
    # Set rules for naming, depending on either there is interaction between 
    # independent variables or not

    if(interaction == "+"){
      model_name <- paste0(prefix, "_", independent_var1, "_", independent_var2)
      } else {
        model_name <- paste0(prefix, "_", independent_var1, "*", independent_var2)
      }
    
    # Add the model name to the model.names vector
    model.names <- c(model.names, model_name)
  }
  
  # Access the model names using the index of the model.names vector
  # For example, to access the name of the model for the combination of the 
  # first independent variables:
  # model.names[1]
  
  return(list(models_list, model.names))
}