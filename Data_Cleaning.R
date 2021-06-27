library(tidyverse)
library(lubridate)
library(modelr)
library(psych)
library(car)
library(GGally)
library(ggpubr)
library(ggrepel)
library(purrr)
library(stringr)

# Importing my data sets
air_data_world <- read_csv("E:\\MY COLLEGE\\ISI KOLKATA\\1ST YEAR\\PROJECTS\\2ND SEM\\STATISTICAL METHODS II\\Air Quality Index\\all_air_data.csv", comment = '#')
air_data_world

# Separating out the date column for easier analysis
air_data_world <- air_data_world %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  select(Year, Month, Day, Country, City, Specie, count, min, max, median, variance)

# Filtering Data relevant to INDIA
air_data_india <- air_data_world %>%
  filter(Country == "IN")
air_data_india <- air_data_india %>%
  select(Year:Day, City:variance)
air_data_india
air_data_india_withDate <- air_data_world %>%
  filter(Country =="IN")

# Changing some basic flaws in data
correct <- function(x) return(ifelse(x == "wind speed", "wind-speed", x))
air_data_india$Specie <- correct(air_data_india$Specie)
correct <- function(x) return(ifelse(x == "wind gust", "wind-gust", x))
air_data_india$Specie <- correct(air_data_india$Specie)
correct <- function(x) return(ifelse(x == "New Delhi", "Delhi", x))
air_data_india$City <- correct(air_data_india$City)


# Arranging the Data according to Year
air_data_india <- air_data_india %>%
  arrange(Year, Month, Day)
air_data_india

# Trying to separate the column Specie for easier analysis -- widening the data
spec1 <- air_data_india %>%
  build_wider_spec(names_from = Specie, values_from = c(count, min, max, median, variance, Year, Month, Day, City))
spec1
# Separate based on Specie
air_data_india_Specie <- air_data_india %>%
  group_by(Specie) %>%
  mutate(row = row_number()) %>%
  pivot_wider_spec(spec1) %>%
  select(-row) %>%
  select(contains("Year"), contains("Month"), contains("Day"), contains("City"), everything())
air_data_india_Specie

# Separate based on City
spec2 <- air_data_india %>%
  build_wider_spec(names_from = City, values_from = c(Specie, count, min, max, median, variance, Year, Month, Day, City))
spec2

air_data_india_City <- air_data_india %>%
  group_by(City) %>%
  mutate(row = row_number()) %>%
  pivot_wider_spec(spec2) %>%
  select(-row) %>%
  select(contains("Year"), contains("Month"), contains("Day"), contains("City"), everything())
air_data_india_City

# Filtering out the pollutants only
air_data_india_pollutants <- air_data_india %>%
  filter(Specie %in% c('pm25', 'pm10', 'o3', 'so2', 'no2', 'co'))

# Filtering out the non-pollutants
air_data_india_nonpollutants <- air_data_india %>%
  filter(!(Specie %in% c('pm25', 'pm10', 'o3', 'so2', 'no2', 'co')))

# What Columns are present in my Data?
colnames(air_data_india)
unique(air_data_india$City)
unique(air_data_india$Specie)

# Making the List of the Stations
station_list <- readxl::read_excel("E:\\MY COLLEGE\\ISI KOLKATA\\1ST YEAR\\PROJECTS\\2ND SEM\\STATISTICAL METHODS II\\Air Quality Index\\Station_List.xlsx", skip = 1)
colnames(station_list) <- c("Sl.No.State", "State", "Sl.No.City", "City", "Sl.No.Station", "Station")
station_list <- station_list %>%
  select(State, City, Station) %>%
  fill(State) %>%
  fill(City) %>%
  fill(Station)
station_list

station_list <- station_list %>%
  filter(City %in% unique(air_data_india$City))

station_list <- station_list %>%
  filter(City %in% unique(air_data_india$City)) %>%
  group_by(State, City) %>%
  summarise("Number of Stations" = n())
