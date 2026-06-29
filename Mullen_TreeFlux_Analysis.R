# Analysis of the 2021-2025 TEMPEST Flood Tree Greenhouse Gas flux dataset 
# Julianna Mullen 2026

message("Welcome to the TEMPEST Flood Tree Greenhouse Gas Flux dataset analysis")

library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(arrow)
library(lubridate)
library(compasstools)

# Flood dates

flood_date <- tribble(~Event, ~Start, ~End,
                      "T1", "2022-06-22", "2022-06-22",
                      "T2", "2023-06-06", "2023-06-07",
                      "T3", "2024-06-11", "2024-06-13")
flood_date$Start <- ymd(flood_date$Start)
flood_date$End <- ymd(flood_date$End)

# Read in data from Apache Parquet file -----
my_data <- read_parquet("tempest_tree_ghg_fluxes.parquet")
code_to_name <- c(ACRU = "Red Maple", FAGR = "Beech", LITU = "Tulip Poplar")

tree_map <- read.csv("ancillary_data/sapflow_tree_mapping.csv",
                     stringsAsFactors = FALSE) %>%
  mutate(across(where(is.character), trimws)) %>%
  transmute(ID = Sapflux_ID,
            Species_from_map = unname(code_to_name[Species_Code])) %>%
  distinct(ID, .keep_all = TRUE)

my_data <- my_data %>%
  left_join(tree_map, by = "ID") %>%
  mutate(Species = coalesce(Species, Species_from_map)) %>%
  select(-Species_from_map)

sum(is.na(my_data$Species))

# Prep Soil Temp Data
read_L2_variable(variable = "soil-temp-5cm", path = "Sensor_Data/", quiet=TRUE)
## Take daily avg
soil_temp_data <- read_L2_variable(variable = "soil-temp-5cm",
    path = "Sensor_Data/", quiet = TRUE)
soil_temp_daily <- soil_temp_data %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP),
    Date = as.Date(TIMESTAMP),
    Value = as.numeric(Value) ) %>%
  filter(!is.na(Value)) %>%  
  group_by(Site, Plot, Date) %>%
  summarise(
    soil_temp_daily_mean = mean(Value),
    n = n(),
    coverage = n / 480,      
    .groups = "drop"  )
print(soil_temp_daily)

##Graphs of Soil Temp Data (Daily averages over time)
ggplot(soil_temp_daily,aes(x = Date, y = soil_temp_daily_mean,
color = Plot, group = Plot)) + geom_line(alpha = 0.7) + facet_wrap(~ Site) +
labs( title = "Daily Mean Soil Temperature by Plot",
   x = "Date",
   y = "Soil Temperature (°C)" ) 

# Numerical summaries -----

print(my_data)
# BBL: print summary of fluxes? A table with mean, min, max and date ranges

##____________________________________________________________________________

#Initial Visualization of Raw Data (CH4)
my_data %>%
  ggplot(aes( x = Year, y = CH4_lin_flux.estimate, color = Plot )) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Species) +
  labs( title = "CH4 Flux by Species (2021–2025)" )

#Initial Visualization of Raw Data (CO2)
my_data %>%
  ggplot(aes( x = Year, y = CO2_lin_flux.estimate, color = Plot )) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Species) +
  labs( title = "CO2 Flux by Species (2021–2025)", y = "CO2 Flux" )

# Overall flux range (using boxplot) for each gas, by tree species
ggplot(my_data,aes(x = Species, y = CH4_lin_flux.estimate, fill = Plot)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(my_data, aes(x = Species,  y = CO2_lin_flux.estimate, fill = Plot)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Quarterly Flux for both GHG (raw data)
annual_means_CH4 <- my_data %>%
  group_by(Year, Species, Plot) %>%
  summarize( mean_flux = mean(CH4_lin_flux.estimate, na.rm = TRUE),
             se = sd(CH4_lin_flux.estimate, na.rm = TRUE) / sqrt(n()), .groups = "drop")
ggplot(annual_means_CH4, aes( x = Year, y = mean_flux,color = Plot,group = Plot)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar( aes(  ymin = mean_flux - se,ymax = mean_flux + se  ),width = 0.1 ) +
  facet_wrap(~ Species) +
  labs( title = "Mean CH4 Flux by Species and Plot (2021–2025)",
        x = "Year", y = "Mean CH4 Flux" ) 

annual_means_CO2 <- my_data %>%
  group_by(Year, Species, Plot) %>%
  summarize( mean_flux = mean(CO2_lin_flux.estimate, na.rm = TRUE),
             se = sd(CO2_lin_flux.estimate, na.rm = TRUE) / sqrt(n()), .groups = "drop")

ggplot(annual_means_CO2,aes( x = Year, y = mean_flux, color = Plot, group = Plot  )) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar( aes(ymin = mean_flux - se,ymax = mean_flux + se ), width = 0.1) +
  facet_wrap(~ Species) +
  labs( title = "Mean CO2 Flux by Species and Plot (2021–2025)",
        x = "Year",
        y = "Mean CO2 Flux" ) 

## Spaghetti plots of Raw Data
ggplot(my_data, aes(x = Date, y = CH4_lin_flux.estimate,
                    group = ID, color = Plot)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~ Species) +
  labs(title = "CH4 Flux Over Time by Species and Plot",
       x = "Date",
       y = "CH4 Flux")

ggplot(my_data, aes(x = Date,
                    y = CO2_lin_flux.estimate,
                    group = ID,
                    color = Plot)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~ Species) +
  labs(
    title = "CO2 Flux Over Time by Species and Plot",
    x = "Date",
    y = "CO2 Flux" )

## Heat Maps 
heat_data_CH4 <- my_data %>%
  group_by(Species, Year) %>%
  summarize(
    mean_flux = mean(CH4_lin_flux.estimate, na.rm = TRUE),
    .groups = "drop" )
ggplot(heat_data_CH4, aes(x = Year, y = Species, fill = mean_flux)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs( title = "Mean CH4 Flux by Species and Year", x = "Year", y = "Species",
        fill = "Mean CH4 Flux") + theme_minimal()
seawater_rows <- my_data %>%
  filter(Plot == "Seawater")

heat_data_CO2 <- my_data %>%
  group_by(Species, Year) %>%
  summarize(mean_flux = mean(CO2_lin_flux.estimate, na.rm = TRUE),.groups = "drop")
ggplot(heat_data_CO2,aes(x = Year, y = Species, fill = mean_flux)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Mean CO2 Flux by Species and Year", x = "Year",y = "Species",
       fill = "Mean CO2 Flux" ) +theme_minimal()
ggplot(my_data, aes(x = CO2_lin_flux.estimate, y = CH4_lin_flux.estimate, color = Species)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between CO2 and CH4 Flux by Species",x = "CO2 Flux",y = "CH4 Flux",
       color = "Species") +
  theme_minimal()
## ____________________________________________________________________________
# Creation of Quarters
my_data <- my_data %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Quarter = quarter(Date),
    YearQuarter = Year + (Quarter-1)/4)

table(my_data$Quarter)

quarterly_CH4 <- my_data %>%
  mutate(is_S7 = ID == "S7") %>%
  group_by(YearQuarter, Species, Plot, is_S7) %>%
  summarize(mean_flux = mean(CH4_lin_flux.estimate, na.rm = TRUE),
            se = sd(CH4_lin_flux.estimate, na.rm = TRUE) / sqrt(n()), .groups = "drop")

# We want flood-week averages for reference
flood_data <- list()
for(i in seq_len(nrow(flood_date))) {
  message("Isolating data for ", flood_date$Event[i])
  flood_data[[i]] <-
    my_data %>%
    filter(
      Date >= flood_date$Start[i],
      Date <= flood_date$End[i]  ) %>%
    mutate(Event = flood_date$Event[i])}

# Now we have the data for only flood days; summarize just like above
flood_CH4 <- bind_rows(flood_data) %>%
  mutate(is_S7 = ID == "S7") %>%
  group_by(Event, YearQuarter, Species, Plot, is_S7) %>%
  summarize(
    mean_flux = mean(CH4_lin_flux.estimate, na.rm = TRUE),
    se = sd(CH4_lin_flux.estimate, na.rm = TRUE) / sqrt(n()),
    .groups = "drop" )

#Quarterly Methane Flux with flood averages emphasized
ggplot(quarterly_CH4,
       aes(x = YearQuarter, y = mean_flux, color = Plot, group = Plot)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_flux - se,
        ymax = mean_flux + se),
    width = 0.05) +
  # Flood overlay (T1, T2, T3 labeled in legend)
  geom_point(
    data = flood_CH4,
    aes(x = YearQuarter,
        y = mean_flux,
        shape = Event),
    color = "black",
    size = 1.5,
    inherit.aes = FALSE) +
  facet_wrap(~ Species) +
  labs( title = "Quarterly CH4 Flux with Flood Events Highlighted",
        x = "Year-Quarter",
        y = "CH4 Flux",
        shape = "Flood event") +
  scale_shape_manual(values = c(
    T1 = 17,
    T2 = 17,
    T3 = 17)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
## ____________________________________________________________________________

#isolating Red Maple Trees (finding outlier (S7))
my_data %>%
  filter(Species == "Red Maple") %>%
  ggplot(aes(x = Date, y = CH4_lin_flux.estimate)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ ID) +
  labs(title = "Red Maple CH4 Flux by Individual Tree")
my_data %>%
  filter(Species == "Red Maple") %>%
  ggplot(aes(x = Date, y = CO2_lin_flux.estimate)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ ID) 

#S7 Drama _______________________________________________
# Separate S7 from all other trees
s7_CH4 <- my_data %>%
  filter(ID == "S7") %>%
  group_by(YearQuarter, Species, Plot) %>%
  summarize(mean_flux = mean(CH4_lin_flux.estimate, na.rm = TRUE),
            se = sd(CH4_lin_flux.estimate, na.rm = TRUE) / sqrt(n()), .groups = "drop"  )
quarterly_CH4_nos7 <- my_data %>%
  filter(ID != "S7") %>%
  group_by(YearQuarter, Species, Plot) %>%
  summarize(mean_flux = mean(CH4_lin_flux.estimate, na.rm = TRUE),
            se = sd(CH4_lin_flux.estimate, na.rm = TRUE) / sqrt(n()), .groups = "drop" )
quarterly_CH4_S7 <- s7_CH4
flood_points <- flood_date %>%
  mutate(YearQuarter = year(Start) + (quarter(Start)-1)/4) %>%
  select(Event, YearQuarter)
flood_S7_CH4 <- s7_CH4 %>%
  inner_join(flood_points, by = "YearQuarter")

#Quarterly CH4 Flux (S7 excluded from means, shown separately)
ggplot() +
  # OTHER TREES (baseline)
  geom_line(
    data = quarterly_CH4_nos7,
    aes(x = YearQuarter, y = mean_flux, color = Plot, group = Plot)) +
  geom_point(
    data = quarterly_CH4_nos7,
    aes(x = YearQuarter, y = mean_flux, color = Plot)) +
  # S7 (highlighted)
  geom_line(
    data = s7_CH4,
    aes(x = YearQuarter, y = mean_flux),
    color = "red",
    linewidth = 1 ) +
  geom_point(data = s7_CH4,
             aes(x = YearQuarter, y = mean_flux),
             color = "red", size = 2) +
  # FLOOD EVENTS ON S7 
  geom_point(
    data = s7_CH4 %>%
      inner_join(flood_points, by = "YearQuarter"),
    aes(x = YearQuarter, y = mean_flux, shape = Event),
    color = "black",
    size = 3 ) +
  facet_wrap(~ Species) +
  labs( title = "Quarterly CH4 Flux (S7 excluded from means, shown separately)",
        x = "Year-Quarter",
        y = "CH4 Flux",
        shape = "Flood event") +
  scale_shape_manual(values = c(
    T1 = 17,
    T2 = 17,
    T3 = 17 )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#S7 only 
ggplot(quarterly_CH4_S7, aes(x = YearQuarter, y = mean_flux)) +
  geom_line(color = "red", linewidth = 1) + geom_point(color = "red", size = 2) +
  geom_errorbar(aes(ymin = mean_flux - se, ymax = mean_flux + se),
                width = 0.05, color = "red" ) +
  geom_point(
    data = flood_S7_CH4,
    aes(x = YearQuarter, y = mean_flux),
    color = "black",
    size = 2  ) +
  labs( title = "S7 Quarterly CH4 Flux with Flood Periods Highlighted",
        x = "Year-Quarter",
        y = "CH4 Flux") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#S7 Excluded from CH4 Flux
flood_plot_data <- quarterly_CH4_nos7 %>%
  inner_join(flood_points, by = "YearQuarter")
ggplot() +  geom_line(
  data = quarterly_CH4_nos7,
  aes(x = YearQuarter, y = mean_flux, color = Plot, group = Plot) ) +
  geom_point(
    data = quarterly_CH4_nos7,aes(x = YearQuarter, y = mean_flux, color = Plot) ) +
  geom_point(data = flood_plot_data,
             aes(x = YearQuarter, y = mean_flux, shape = Event),
             color = "black",
             size = 3 ) +
  facet_wrap(~ Species) +
  labs(title = "Quarterly CH4 Flux with Flood Events Across Treatments",
       x = "Year-Quarter",
       y = "CH4 Flux",
       shape = "Flood event" ) +
  scale_shape_manual(values = c(T1 = 17, T2 = 17, T3 = 17)) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))