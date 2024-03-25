# Custom function that calculate Growing degree units (GDU, Sum of the mean daily temperature above 0 from the start of the year) for each day of the year.
# Must be calculated for each year separately.
# Data must contain one row per day and the variable with the positive daily mean temperature.
# If mean temperature is negative, put zero in this variable.
# Also the variable with the full date MUST be character AND named "date"
gdu <- function(daily_data_for_one_year, 
                var_with_PositiveMeanTemp) {
  GDU <- list() # prepare empty list
  
  # looping through the daily data within the year 
  for (i in 1:nrow(daily_data_for_one_year)) {
    GDU[[i]] <- c(daily_data_for_one_year$date[i],
                  sum(daily_data_for_one_year[1:i, var_with_PositiveMeanTemp])
    )
  }
  # reshape obtained list
  GDU <- as.data.frame(GDU)
  GDU <- as.data.frame(t(GDU))
  colnames(GDU) <- c("date", "GDU")
  # retrieve the result
  return(GDU)
}