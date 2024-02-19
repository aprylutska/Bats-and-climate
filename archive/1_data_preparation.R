rm(list = ls()) # reset R's brain

setwd("~/R/Alona/homilsha") # change path to yours

df <- read.csv("Homolsha_all_years.csv") # read data from *.csv file

# Set variables classes
df$date <- as.Date(df$Data, "%d.%m.%Y") # convert date to R Data format
df$Place <- as.factor(df$Place)
df$Species <- as.factor(df$Species)
df$sex <- as.factor(df$sex)
df$age <- as.factor(df$age)

# Delete unused variables
df$Data <- NULL
df$reproductive.status <- NULL

# Take a look at the data
summary(df)

# Check `sex` variable - odd level "sad\n".
df[df$age == "sad\n",]

# Take ONE of the following action:
# df <- df[df$age != "sad\n", ] # 1. Remove an entire row based on condition
df$age[df$age == "sad\n"] <- "sad" # 2. Change "sad\n" to "sad"

# Subset only some species (e.g., N.noc. and P.pygm.)
# df <- subset(df, Species %in% c('Nyctalus noctula', 'Pipistrellus pygmaeus'))

# Save cleaned data
# As csv (variable classes will be lost)
write.csv(df, file = "Homolsha_all_years_cleaned.csv") # not recommended

# As Rdata object (all changes will be kept)
save(df, file = "Homolsha_all_years_cleaned.Rdata")

# You can upload saved Rdata using
load(file = "Homolsha_all_years_cleaned.Rdata")


################################################################################

# Generate weather data
# Take a look at the timespan
summary(df$date)

# Install `easyclimate` package from GitHub, if not yet
# install.packages("remotes")
# remotes::install_github("VeruGHub/easyclimate")
# Don't forget to comment two rows above after installation!

# Load libraries
library(tidyverse) # for data manipulations
library(easyclimate) # for daily climate data extraction
library(sf) # for spatial operations

# Create data frame with point coordinates
coords <- data.frame(
  place = c("R1", "R2", "Fed1", "Fed2", "Fed3", "F1", "F2", "F3", "F4"),
  lon = c(36.33329, 36.32946, 36.32732, 36.32863, 36.33436, 36.32396, 36.32319, 36.31990, 36.31909),
  lat = c(49.62554, 49.62092, 49.62298, 49.61411, 49.60477, 49.61782, 49.61169, 49.60948, 49.60782)
)

# Retrieve daily weather values using {easyclimate} package
# Prcp - Total precipitation in mm
# Tmin - minimal temperature, Celsius degrees
# Tmax - maximal temperature, Celsius degrees

daily <- get_daily_climate(
  coords = coords,
  period = 2008:2011,
  climatic_var = c("Prcp","Tmin","Tmax"),
  output = "df")

save(daily, file = "daily_weather_2008-2011") # save weather data
load(file = "daily_weather_2008-2011")


sessionInfo() # to check your locale

# if LC_TIME=uk_UA.UTF-8, your months will be in Ukrainian.
# To fix that, you need to re-map locate into US (will be back after restart R session)

# Set locale to get English months names
Sys.setlocale("LC_TIME", "en_US.UTF-8") # for Linux
# Sys.setlocale("LC_TIME", "English") # for Windows

daily <- daily |>
  group_by(date) |>                          # Group all places by date
  summarise(Prcp = sum(Prcp, na.rm = TRUE),  # aggregate values by date
            Tmin = mean(Tmin),
            Tmax = mean(Tmax)) |>
  mutate(DiurnialRange = Tmax-Tmin,          # Calculate derived metrics
         MeanTemp = (Tmin + Tmax)/2,
         PositiveMeanTemp = ifelse(MeanTemp > 0, MeanTemp, 0)) |> # Get only positive mean temperatures (for Growing degree units calculation)
  mutate(date = as.Date(date),
         month = months(date),
         year = format(date, format = "%Y")) # Adjust dates and add new variables for month and years


# Take a look at the minimal temperatures of April in 2008-2011
# temp_long <- daily %>%
#   pivot_longer(
#     cols = "Tmin",
#     names_to = "temp_vars",
#     values_to = "temp_values")

daily |>
  filter(month == "April") |>
  select(date, year, Tmin) |>
  ggplot(aes(Tmin)) +
  geom_density(fill = "lightgreen", 
               alpha = 0.5) +
  facet_wrap( ~ factor(year)) +
  labs(title = "April",
       x = "Minimal temperature") +
  theme_bw()

ggsave("Tmin_2008-2011_density_Apr.png", width = 16, height = 12, units = "cm", dpi = 150)


daily |>
  filter(month == "April") |>
  select(date, year, Tmin) |>
  ggplot(aes(year, Tmin)) +
  geom_boxplot(fill = "lightgreen", 
               alpha = 0.5) +
  labs(title = "April",
       x = "years",
       y = "Minimal temperature") +
  theme_bw()

ggsave("Tmin_2008-2011_boxplot_Apr.png", width = 16, height = 12, units = "cm", dpi = 150)



daily |>
  filter(month %in% c("March", "April", "May", "June")) |>
  select(date, year, month, Tmin) |>
  ggplot(aes(year, Tmin)) +
  geom_boxplot(fill = "lightgreen", 
               alpha = 0.5) +
  facet_wrap( ~ factor(month)) +
  labs(x = "years",
       y = "Minimal temperature") +
  theme_bw()

ggsave("Tmin_2008-2011_boxplot_Mar-Jun.png", width = 16, height = 12, units = "cm", dpi = 150)


# Calculate GDU
# Growing degree units (Sum of the mean daily temperature above 0 from the start of the year)
daily$date <- as.character(daily$date) # loop doesn't seem to work with `date` format

GDU <- list() # prepare empty list

for (i in 1:nrow(daily)) {
  GDU[[i]] <- c(daily$date[i], sum(daily$PositiveMeanTemp[1:i]))
}

GDU <- as.data.frame(GDU)
GDU <- as.data.frame(t(GDU))
colnames(GDU) <- c("date", "GDU")

# Merge derived climate data
climate <- daily |>                     # direct daily measurements
  full_join(GDU, by = "date") |>        # Growing degree units
  mutate(date = as.Date(date)) |>       # keep date as Date
  mutate(month = as.factor(month)) |>
  mutate(GDU = as.numeric(GDU))
  

# Export climate data for further sessions
save(climate, file = "climate_2008-2011.Rdata")

# Merge climate data with bats data

df_clim <- df |>
  left_join(climate, by = "date")

# Export climate data for further sessions
save(df_clim, file = "bats_wClimate_2008-2011.Rdata")

