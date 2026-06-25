# Analysis of the 2021-2025 TEMPEST Flood Tree Greenhouse Gas flux dataset 
# Julianna Mullen 2026

message("Welcome to the TEMPEST Flood Tree Greenhouse Gas Flux dataset analysis")

library(ggplot2)
theme_set(theme_bw())
library(dplyr)
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
library(arrow)
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

# Numerical summaries -----

print(my_data)
# BBL: print summary of fluxes? A table with mean, min, max and date ranges


 my_data %>%
   ggplot(aes( x = Year, y = CH4_lin_flux.estimate, color = Plot )) +
   geom_point(alpha = 0.6) +
   facet_wrap(~ Species) +
   labs( title = "CH4 Flux by Species (2021–2025)" )

my_data %>%
  ggplot(aes( x = Year, y = CO2_lin_flux.estimate, color = Plot )) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Species) +
  labs( title = "CO2 Flux by Species (2021–2025)", y = "CO2 Flux" )


# Overall flux range (using boxplot) for each gas, by tree species
ggplot(my_data,aes(x = Species, y = CH4_lin_flux.estimate, fill = Plot)) +
   geom_boxplot() +
  theme_bw() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(my_data, aes(x = Species,  y = CO2_lin_flux.estimate, fill = Plot)) +
    geom_boxplot() +
   theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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
        x = "Year", y = "Mean CH4 Flux" ) +
  theme_bw()

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
    y = "Mean CO2 Flux" ) +
  theme_bw()

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
    y = "CO2 Flux"
  )

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

ggplot(my_data, aes(x = Plot,
                    y = CH4_lin_flux.estimate,
                    fill = Plot)) +
  geom_boxplot() +
  facet_wrap(~ Species) +
  labs(
    title = "CH4 Flux by Plot and Species",
    x = "Plot",
    y = "CH4 Flux",
    fill = "Plot" )

ggplot2::ggplot(my_data, aes(x = Plot, y = CO2_lin_flux.estimate, fill = Plot)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~ Species) +
  ggplot2::labs(
    title = "CO2 Flux by Plot and Species",
    x = "Plot",
    y = "CO2 Flux",
    fill = "Plot" )


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
  my_data %>%
    filter(Date >= flood_date$Start[i], Date <= flood_date$End[i]) ->
    flood_data[[i]]
}
# Now we have the data for only flood days; summarize just like above
flood_CH4 <- bind_rows(flood_data) %>%
  mutate(is_S7 = ID == "S7") %>%
  group_by(YearQuarter, Species, Plot, is_S7) %>%
  summarize(mean_flux = mean(CH4_lin_flux.estimate, na.rm = TRUE),
            se = sd(CH4_lin_flux.estimate, na.rm = TRUE) / sqrt(n()), .groups = "drop")


# my_data <- my_data %>%
#   mutate( YearQuarter = paste0(Year, "-Q", Quarter))
ggplot(quarterly_CH4,
       aes(x = YearQuarter,
           y = mean_flux,
           color = Plot,
           linetype = is_S7)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Species) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs( x = "Year-Quarter",y = "CH4 Flux",title = "Quarterly CH4 Flux (2021–2025)" ) +
  # show flood week data
  geom_point(data = flood_CH4, size = 4)

my_data %>%
  filter(Species == "Red Maple") %>%
  group_by(ID) %>%
  summarize(
    n = n(),
    mean_CH4 = mean(CH4_lin_flux.estimate, na.rm = TRUE),
    sd_CH4 = sd(CH4_lin_flux.estimate, na.rm = TRUE),
    mean_CO2 = mean(CO2_lin_flux.estimate, na.rm = TRUE),
    sd_CO2 = sd(CO2_lin_flux.estimate, na.rm = TRUE)
  ) %>%
  arrange(desc(abs(mean_CH4)))

my_data %>%
   filter(Species == "Red Maple") %>%
    ggplot(aes(x = Date, y = CH4_lin_flux.estimate)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ ID) +
   theme_bw() +
   labs(title = "Red Maple CH4 Flux by Individual Tree")
 my_data %>%
   filter(Species == "Red Maple") %>%
    ggplot(aes(x = Date, y = CO2_lin_flux.estimate)) +
   geom_line(alpha = 0.7) +
   facet_wrap(~ ID) +
   theme_bw()

 