# Custom function to find spring onset.
# We define spring onset as the final day of the first two-week period when mean daily temperature is constantly above 0 Celsius degree.
spring_onset_day <- function(data, var_with_temperature){
  dt <- data
  library(data.table)
  setDT(dt)
  
  # value constraints
  # dt$value_cons_met <- dt$var_with_temperature > 0
  dt$value_cons_met <- dt[[var_with_temperature]] > 0
  
  
  # assign all potential sequential true time stamps a group id
  dt$potential_win_id <- c(0,cumsum(abs(diff(dt$value_cons_met))))
  
  # is the window big enough?
  dt[,window_size_okay := max(date)-min(date) >= 14 , by = potential_win_id]
  # Other window dependent constraints can be put here
  
  # Window "ID" is defined if the valid cons are met and window size is okay
  # in that case copy potential window number as window id
  dt[,window_id := ifelse(value_cons_met & window_size_okay, potential_win_id, NA)]
  
  # Add 14 days forward to the start date of the first appropriate time window in a year
  SprOn <- yday(dt[!is.na(window_id), min(date) + 14])
  
  return(SprOn)
  
}