# ============================================================
# Analysis of the 2021-2025 TEMPEST Flood Tree Greenhouse Gas flux dataset
# Julianna Mullen 2026
# ============================================================

message("Welcome to the TEMPEST Flood Tree Greenhouse Gas Flux dataset analysis")

library(ggplot2)
theme_set(theme_bw())

# ============================================================
# Load Required Packages
# ============================================================

# Read in data from Apache Parquet file -----

library(dplyr)

# ============================================================
# Species-Level Flux Distributions
# Purpose: Compare the range and variability of CH4 and CO2
# fluxes among species and flooding treatments.
# ============================================================

ggplot(my_data,aes(x = Species, y = CH4_lin_flux.estimate, fill = Plot)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(my_data, aes(x = Species,  y = CO2_lin_flux.estimate, fill = Plot)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================
# Annual Mean CH4 Flux Trends
# Purpose: Summarize yearly changes in CH4 flux by species
# and flooding treatment.
# ============================================================

annual_means_CH4 <- my_data %>%
  group_by(Year, Species, Plot) %>%
  summarize( mean_flux = mean(CH4_lin_flux.estimate, na.rm = TRUE),
  se = sd(CH4_lin_flux.estimate, na.rm = TRUE) / sqrt(n()), .groups = "drop")

ggplot(annual_means_CH4, aes( x = Year, y = mean_flux,color = Plot,group = Plot)) +
  geom_line() + geom_point(size = 2) + geom_errorbar(aes
(ymin = mean_flux - se,ymax = mean_flux + se  ),width = 0.1 ) +
  facet_wrap(~ Species) + labs( title = "Mean CH4 Flux by Species and Plot (2021–2025)", 
x = "Year", y = "Mean CH4 Flux" ) 

# ============================================================
# Annual Mean CO2 Flux Trends
# Purpose: Summarize yearly changes in CO2 flux by species
# and flooding treatment.
# ============================================================

annual_means_CO2 <- my_data %>%
  group_by(Year, Species, Plot) %>%
  summarize( mean_flux = mean(CO2_lin_flux.estimate, na.rm = TRUE),
   se = sd(CO2_lin_flux.estimate, na.rm = TRUE) / sqrt(n()), .groups = "drop")

ggplot(annual_means_CO2,aes( x = Year, y = mean_flux, color = Plot, group = Plot  )) +
  geom_line() + geom_point(size = 2) +geom_errorbar(aes
  (ymin = mean_flux - se,ymax = mean_flux + se ), width = 0.1) +
  facet_wrap(~ Species) +labs( title = "Mean CO2 Flux by Species and Plot (2021–2025)",
 x = "Year", y = "Mean CO2 Flux" ) +

# ============================================================
# Individual Tree CH4 Flux Through Time
# Purpose: Visualize temporal variation in CH4 flux for
# individual tree IDs across species and treatments.
# ============================================================

ggplot(my_data, aes(x = Date,  y = CH4_lin_flux.estimate, group = ID, color = Plot)) +
  geom_line(alpha = 0.2) + facet_wrap(~ Species) +
  labs(title = "CH4 Flux Over Time by Species and Plot", x = "Date", y = "CH4 Flux")

# ============================================================
# Individual Tree CO2 Flux Through Time
# Purpose: Visualize temporal variation in CO2 flux for
# individual tree IDs across species and treatments.
# ============================================================

ggplot(my_data, aes(x = Date,  y = CO2_lin_flux.estimate, group = ID, color = Plot)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~ Species) +
  labs(title = "CO2 Flux Over Time by Species and Plot", x = "Date", y = "CO2 Flux")

# ============================================================
# CH4 Heatmap by Species and Year
# Purpose: Identify annual patterns and species-level
# differences in average CH4 flux.
# ============================================================

heat_data_CH4 <- my_data %>%
  group_by(Species, Year) %>%
  summarize(mean_flux = mean(CH4_lin_flux.estimate, na.rm = TRUE), .groups = "drop" )

ggplot(heat_data_CH4, aes(x = Year, y = Species, fill = mean_flux)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Mean CH4 Flux by Species and Year", x = "Year", y = "Species",
       fill = "Mean CH4 Flux" ) +
  theme_minimal()

# ============================================================
# CO2 Heatmap by Species and Year
# Purpose: Identify annual patterns and species-level
# differences in average CO2 flux.
# ============================================================

heat_data_CO2 <- my_data %>%
  group_by(Species, Year) %>%
  summarize(mean_flux = mean(CO2_lin_flux.estimate, na.rm = TRUE),.groups = "drop")

ggplot(heat_data_CO2,aes(x = Year, y = Species, fill = mean_flux)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Mean CO2 Flux by Species and Year", x = "Year",y = "Species",
       fill = "Mean CO2 Flux" ) +
  theme_minimal()

# ============================================================
# Relationship Between CO2 and CH4 Fluxes
# Purpose: Examine whether CO2 and CH4 fluxes are correlated
# and whether relationships differ among species.
# ============================================================

ggplot(my_data, aes(x = CO2_lin_flux.estimate, y = CH4_lin_flux.estimate, color = Species)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between CO2 and CH4 Flux by Species",x = "CO2 Flux",y = "CH4 Flux",
       color = "Species") +
  theme_minimal()

# ============================================================
# Treatment Effects on CH4 Flux Within Species
# Purpose: Compare CH4 flux distributions among flooding
# treatments for each species.
# ============================================================

ggplot(my_data, aes(x = Plot, y = CH4_lin_flux.estimate, fill = Plot)) +
  geom_boxplot() +
  facet_wrap(~ Species) +
  labs( title = "CH4 Flux by Plot and Species",x = "Plot",y = "CH4 Flux", fill = "Plot")

# ============================================================
# Treatment Effects on CO2 Flux Within Species
# Purpose: Compare CO2 flux distributions among flooding
# treatments for each species.
# ============================================================

ggplot(my_data, aes(x = Plot, y = CO2_lin_flux.estimate, fill = Plot)) +
  geom_boxplot() +
  facet_wrap(~ Species) +
  labs(title = "CO2 Flux by Plot and Species",x = "Plot", y = "CO2 Flux", fill = "Plot")

# ============================================================
# Overall Mean CO2 Flux Through Time
# Purpose: Visualize average CO2 flux trends across the
# study period for each treatment.
# ============================================================

ggplot(my_data,aes(Date, CO2_lin_flux.estimate,color = Plot)) +
  stat_summary(fun = mean,   geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar",  width = 0)

# ============================================================
# Overall Mean CH4 Flux Through Time
# Purpose: Visualize average CH4 flux trends across the
# study period for each treatment.
# ============================================================

ggplot(my_data,aes(Date, CH4_lin_flux.estimate, color = Plot)) +
  stat_summary(fun = mean,geom = "line",     linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0)
