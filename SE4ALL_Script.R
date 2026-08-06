## ----setup, include = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, results = "hide")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse) # for data visualization and manipulation
library(ggnewscale) # for allowing more than one color scale for plots
library(scales) # for scaling logic in plots
library(patchwork) # stacks plots vertically

library(naniar) # for summarizing missing data
library(mice) # for logistic imputation
library(Amelia) # for multivariate time-series imputation
library(tsibble) # for allowing tibbles to handle time-series data
library(feasts) # for providing time-series analysis tools
library(imputeTS) # for imputing in time-series data

library(countrycode) # for standardizing country codes
library(maps) # for creating maps
library(zoo) # for handling time-series analysis
library(knitr) # knitting documents and tibbles

library(plm) # for linear panel data model analysis
library(lmtest) #  for coeftest parsing
library(sandwich) # for clustered robust vcov matrices

energy <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-26/energy_cleaned.csv")
head(energy)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
energy %>% 
  distinct(country_name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# "Nothern America" misspelling is deliberate; actual spelling in original dataset
names_to_remove <- c("Caucasus and Central Asia", "Eastern Asia (including Japan)", "Eastern Asia (not including Japan)", "Eastern Europe", "Europe", "High income", "High income: nonOECD", "High income: OECD", "Latin America and Caribbean", "Low & middle income", "Low income", "Lower middle income", "Middle income", "Northern Africa" , "Nothern America", "Oceania", "Oceania (not including Australia and New Zealand)", "South Eastern Asia", "Southern Asia", "Sub-Saharan Africa", "Upper middle income", "Western Asia", "Western Sahara", "World") 

energy <- energy %>%
  filter(!country_name %in% names_to_remove)
energy <- energy %>% rename(year = yr)
energy <- energy %>% rename(country = country_name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
energy <- energy %>%
  rename(
    # access
    rural_access_fuel = access_non_solid_fuel_rural_pop_pct,
    total_access_fuel = access_non_solid_fuel_total_pop_pct,
    urban_access_fuel = access_non_solid_fuel_urban_pop_pct,
    rural_access_elec = access_electricity_rural_pop_pct,
    total_access_elec = access_electricity_total_pop_pct,
    urban_access_elec = access_electricity_urban_pop_pct,
    # indices
    activity_index = divisia_decomp_analysis_activity_component_index,
    efficiency_index = divisia_decomp_analysis_energy_intensity_component_index,
    structure_index = divisia_decomp_analysis_structure_component_index,
    # economics
    final_energy_ppp_mj = energy_intensity_level_final_energy_megajoules_per_usd_2005_ppp,
    primary_energy_ppp_mj = energy_intensity_level_primary_energy_megajoules_per_usd_2005_ppp,
    energy_agri_mj = energy_intensity_agricultural_sector_megajoules_per_usd_2005,
    energy_industrial_mj = energy_intensity_industrial_sector_megajoules_per_usd_2005,
    energy_others_mj = energy_intensity_other_sectors_megajoules_per_usd_2005,
    primary_energy_savings_tj = energy_savings_primary_energy_terajoules,
    fp_ratio_pct = final_to_primary_energy_ratio_pct,
    # renewable
    renewable_output_pct = perc_renewable_of_total_electricity_output,
    renewable_gwh = renewable_energy_electricity_output_gigawatt_hours,
    renewable_capacity_gw = renewable_energy_installed_capacity_gigawatts,
    renewable_capacity_pct = share_of_renewable_capacity_in_total_capacity_pct,
    # efficiency and capacity
    thermal_eff_pct = thermal_efficiency_in_power_supply_pct,
    total_output_gwh = total_electricity_output_gigawatt_hours,
    total_final_consumed_tj = total_final_consumption_terajoules,
    total_final_eu = total_final_energy_consumption_tfec,
    total_capacity_gw = total_installed_generation_capacity_gigawatts,
    total_primary_tj = total_primary_energy_supply_terajoules,
    power_lost_pct = transmission_and_distribution_losses_pct
  ) %>%
  rename_with(~ gsub("_consumption_tfec_pct", "_eu_pct", .x)) %>%
  rename_with(~ gsub("_consumption_terajoules", "_consumed_tj", .x))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
energy <- energy %>%
  mutate(region = countrycode(country, origin = "country.name", destination = "region"))

unique(energy$region)

energy <- energy %>%
  mutate(region = ifelse(is.na(region) & country == "Mayotte", "Sub-Saharan Africa", region)) %>%
  mutate(region = ifelse(is.na(region) & country == "Puerto Rico", "Latin America & Caribbean", region)) %>%
  mutate(region = ifelse(is.na(region) & country == "Reunion", "Sub-Saharan Africa", region))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
vis_miss(energy) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0, size = 6))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
energy_ts <- energy %>% 
  as_tsibble(key = country, index = year)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
access <- energy_ts %>%
  select(country, country_code, year, rural_access_fuel, urban_access_fuel, total_access_fuel, rural_access_elec, urban_access_elec, total_access_elec, region)
access <- access %>%
  filter(!(year > 1990 & year < 2000) & !(year > 2000 & year < 2010))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
highest_access_countries_fuel <- access %>%
  group_by(country) %>%
  filter(min(total_access_fuel, na.rm = TRUE) == 95) %>%
  distinct(country)

perfect_access_countries_elec <- access %>%
  group_by(country) %>%
  filter(min(total_access_elec, na.rm = TRUE) == 100) %>%
  distinct(country)


## ----paged.print = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
lowest_access_countries_fuel <- access %>%
  index_by(year) %>%
  mutate(fuel_rank = min_rank(total_access_fuel)) %>%
  arrange(year, fuel_rank) %>%
  slice_min(order_by = fuel_rank, n = 10) %>%
  ungroup() %>%
  distinct(country)

lowest_access_countries_elec <- access %>%
  index_by(year) %>%
  mutate(elec_rank = min_rank(total_access_elec)) %>%
  arrange(year, elec_rank) %>%
  slice_min(order_by = elec_rank, n = 10) %>%
  ungroup() %>%
  distinct(country)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
world_map <- map_data("world")

# highest

highest_fuel_countries <- unique(highest_access_countries_fuel$country)
highest_elec_countries <- unique(perfect_access_countries_elec$country)
world_map_with_regions <- map_data("world") %>%
  mutate(world_region = countrycode(region, origin = "country.name", destination = "region"))

highlighted_map_highest <- world_map %>%
  mutate(category = case_when(
    region %in% highest_fuel_countries & region %in% highest_elec_countries ~ "Highest Access to Both",
    region %in% highest_fuel_countries ~ "Highest Non-solid Fuel Access",
    region %in% highest_elec_countries ~ "Highest Electricity Access",
    TRUE ~ "None"
  )) %>%
  filter(category != "None")

ggplot() +
  geom_polygon(data = world_map_with_regions,
               aes(x = long, y = lat, group = group, fill = world_region),
               color = "white", linewidth = 0.1) +
  scale_fill_viridis_d(option = "magma", alpha = 0.2, name = "World Regions") +
  
  new_scale_fill() +
  
  geom_polygon(data = highlighted_map_highest, 
               aes(x = long, y = lat, group = group, fill = category)) +
  geom_polygon(data = highlighted_map_highest, 
               aes(x = long, y = lat, group = group, fill = category),
               color = "white", linewidth = 0.05) +
  scale_fill_brewer(palette = "Dark2", name = "Access") +
  labs(title = str_wrap("Countries with Highest Access to Non-solid Fuel and Electricity", width = 40), x= "Longitude", y = "Latitude") +
  
  coord_quickmap()

# lowest

lowest_fuel_countries <- unique(lowest_access_countries_fuel$country)
lowest_elec_countries <- unique(lowest_access_countries_elec$country)

highlighted_map_lowest <- world_map %>%
  mutate(category = case_when(
    region %in% lowest_fuel_countries & region %in% lowest_elec_countries ~ "Lowest Access to Both",
    region %in% lowest_fuel_countries ~ "Lowest Non-solid Fuel Access",
    region %in% lowest_elec_countries ~ "Lowest Electricity Access",
    TRUE ~ "None"
  )) %>%
  filter(category != "None")

ggplot() +
  geom_polygon(data = world_map_with_regions,
               aes(x = long, y = lat, group = group, fill = world_region),
               color = "#ffffff", linewidth = 0.1) +
  scale_fill_viridis_d(option = "magma", alpha = 0.2, name = "World Regions") +
  
  new_scale_fill() +
  
  geom_polygon(data = highlighted_map_lowest, 
               aes(x = long, y = lat, group = group, fill = category)) +
  geom_polygon(data = highlighted_map_lowest, 
               aes(x = long, y = lat, group = group, fill = category),
               color = "white", linewidth = 0.05) +
  scale_fill_brewer(palette = "Dark2", name = "Access") +
  labs(title = str_wrap("Countries with Lowest Access to Non-solid Fuel and Electricity", width = 40), x = "Longitude", y = "Latitude") +
  
  coord_quickmap()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
energy_ts %>% 
  filter(year %in% c(1990, 2000, 2010)) %>%
  ggplot(aes(x = total_access_elec, y = traditional_biomass_eu_pct)) +
  geom_point(aes(color = region), alpha = 0.7) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  facet_grid(. ~ year) +
  labs(
    x = "Electricity Access of Total Population (%)", 
    y = "Traditional Biomass Consumed by End-Users (%)", 
    title = "Biomass Reliance vs. Total Electricity Access",
    subtitle = "Decadal progression comparison",
    color = "Region"
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
energy_ts %>% 
  filter(year %in% c(1990, 2000, 2010)) %>% 
  pivot_longer(                                
    cols = c(rural_access_elec, urban_access_elec),
    names_to = "loc_type",
    values_to = "access_pct"
  ) %>% 
  ggplot(aes(x = access_pct, y = traditional_biomass_consumed_tj, color = loc_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_y_log10() +
  facet_grid(loc_type ~ year) +
  labs(
    title = "Biomass Consumption vs. Electricity Access (Decadal View)",
    x = "Electricity Access (%)",
    y = "Traditional Biomass Consumed (TJ)",
    color = "Location Type"
  )



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
top_5_countries <- energy %>%
  rowwise() %>%
  mutate(total_country_consumption = sum(c_across(ends_with("_consumed_tj")), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(cumulative_consumption = sum(total_country_consumption, na.rm = TRUE)) %>%
  arrange(desc(cumulative_consumption)) %>%
  slice_max(order_by = cumulative_consumption, n = 5) %>%
  pull(country)

complete_indices <- energy %>%
  group_by(country) %>%
  filter(all(!is.na(activity_index) & !is.na(efficiency_index) & !is.na(structure_index))) %>%
  ungroup()

energy_ts %>%
  filter(country %in% top_5_countries) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = structure_index, color = "Economic Structure")) +
  geom_line(aes(y = efficiency_index, color = "Tech Efficiency")) +
  facet_wrap(~country, scales = "free_y") +
  labs(title = "Index Tracking Across Sample Nations")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
energy_milestones <- energy_ts %>% 
  filter(year %in% c(1995, 2000, 2005, 2010))

p1 <- ggplot(energy_milestones, aes(x = structure_index, y = primary_energy_savings_tj)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  scale_y_continuous(
    trans = "pseudo_log", 
    breaks = c(-1e8, 0, 1e8), 
    labels = c("-100M", "0", "100M")
  ) +
  facet_wrap(~year, nrow = 1) + 
  theme_minimal() +
  labs(title = "Driving Factors Over Time", y = "Primary Energy Savings (TJ)", x = "Structure Index")


p2 <- ggplot(energy_milestones, aes(x = efficiency_index, y = primary_energy_savings_tj)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  scale_y_continuous(
    trans = "pseudo_log", 
    breaks = c(-1e8, 0, 1e8), 
    labels = c("-100M", "0", "100M")
  ) +
  facet_wrap(~year, nrow = 1) + 
  theme_minimal() +
  labs(y = "Primary Energy Savings (TJ)", x = "Efficiency Index")

p1 / p2


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tapio_analysis <- energy_ts %>%
  as_tibble() %>% 
  group_by(region, year) %>%
  summarise(
    energy_intensity = mean(final_energy_ppp_mj, na.rm = TRUE),
    total_consumption_tj = sum(total_primary_tj, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(region) %>% 
  arrange(year) %>%
  mutate(
    derived_gdp = (total_consumption_tj * 1000000) / energy_intensity,
    pct_change_consumption = (total_consumption_tj - lag(total_consumption_tj)) / lag(total_consumption_tj),
    pct_change_wealth = (derived_gdp - lag(derived_gdp)) / lag(derived_gdp),
    tapio_elasticity = pct_change_consumption / pct_change_wealth,
 
    decoupling_state = case_when(
      pct_change_wealth > 0  & pct_change_consumption < 0  ~ "Strong Decoupling",
      pct_change_wealth > 0  & pct_change_consumption > 0  & tapio_elasticity < 0.8  ~ "Weak Decoupling",
      pct_change_wealth > 0  & pct_change_consumption > 0  & tapio_elasticity >= 0.8 & tapio_elasticity <= 1.2 ~ "Expansive Coupling",
      pct_change_wealth > 0  & pct_change_consumption > 0  & tapio_elasticity > 1.2  ~ "Expansive Negative Decoupling",
      pct_change_wealth < 0  & pct_change_consumption < 0  & tapio_elasticity > 1.2  ~ "Recessive Decoupling",
      pct_change_wealth < 0  & pct_change_consumption < 0  & tapio_elasticity >= 0.8 & tapio_elasticity <= 1.2 ~ "Recessive Coupling",
      pct_change_wealth < 0  & pct_change_consumption < 0  & tapio_elasticity < 0.8  ~ "Weak Negative Decoupling",
      pct_change_wealth < 0  & pct_change_consumption > 0  ~ "Strong Negative Decoupling",
      TRUE ~ "Undefined"
    )
  ) %>%
  ungroup()

ggplot(tapio_analysis, aes(x = derived_gdp, y = total_consumption_tj, color = region)) +
  geom_path(arrow = arrow(length = unit(0.2, "cm")), alpha = 0.8, linewidth = 0.8) + 
  geom_point(alpha = 0.4, size = 1) +
  scale_x_log10(
    labels = scales::label_scientific(),
    name = "Derived Economic Wealth (GDP PPP USD equivalent)"
  ) +
  scale_y_log10(
    labels = scales::label_scientific(),
    name = "Total Primary Energy Consumption (TJ)"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Corrected Decoupling Trajectories by Region",
    subtitle = "Log-Log scale comparison of energy elasticity",
    color = "Region"
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
decoupling_summary <- tapio_analysis %>%
  filter(!is.na(decoupling_state), decoupling_state != "Undefined") %>%
  group_by(region, decoupling_state) %>%
  tally(name = "years_in_state") %>%
  mutate(share_of_time_pct = round(years_in_state / sum(years_in_state) * 100, 1)) %>%
  arrange(region, desc(share_of_time_pct))

print(decoupling_summary, n = 30)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
energy_base <- energy %>%
  select(country, country_code, year, region)

block_A <- energy %>%
  select(
    country, year, 
    activity_index, efficiency_index, structure_index, fp_ratio_pct,
    final_energy_ppp_mj, primary_energy_ppp_mj
    )

block_B <- energy %>% 
  select(
    country, year,
    rural_access_fuel, total_access_fuel, urban_access_fuel,
    rural_access_elec, total_access_elec, urban_access_elec,
    renewable_capacity_pct, renewable_capacity_gw, total_capacity_gw,
    power_lost_pct, thermal_eff_pct
  )

block_C <- energy %>% 
  select(
    country, year,
    # system totals
    primary_energy_savings_tj, total_final_consumed_tj, total_primary_tj, total_final_eu,
    # output
    renewable_gwh, total_output_gwh,
    # consumed by sectors
    energy_agri_mj, energy_industrial_mj, energy_others_mj,
    # fuel penetration rate (%)
    biogas_eu_pct, geothermal_energy_eu_pct, hydro_energy_eu_pct,
    liquid_biofuels_energy_eu_pct, marine_energy_eu_pct, modern_biomass_energy_eu_pct,
    solar_energy_eu_pct, traditional_biomass_eu_pct, waste_energy_eu_pct, wind_energy_eu_pct,
    # fuel consumption volume (TJ)
    biogas_consumed_tj, geothermal_energy_consumed_tj, hydro_energy_consumed_tj,
    liquid_biofuels_consumed_tj, marine_consumed_tj, modern_biomass_consumed_tj,
    solar_energy_consumed_tj, traditional_biomass_consumed_tj, waste_energy_consumed_tj,
    wind_energy_consumed_tj
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(zoo)
amelia_A <- amelia(
  x = as.data.frame(block_A),
  m = 1,
  cs = "country",
  ts = "year",
  polytime = 2 # allows smooth structural curvature over time
)
block_A_clean <- amelia_A$imputations$imp1

matrix_B <- block_A_clean %>%
  inner_join(block_B, by = c("country", "year")) %>%
  mutate(across(where(is.character), as.factor))

b_pct_cols <- c("rural_access_fuel", "total_access_fuel", "urban_access_fuel",
                "rural_access_elec", "total_access_elec", "urban_access_elec",
                "renewable_capacity_pct", "power_lost_pct", "thermal_eff_pct")

bounds_B_matrix <- matrix(NA, nrow = length(b_pct_cols), ncol = 3)
for(i in seq_along(b_pct_cols)) {
  # sets bounds
  bounds_B_matrix[i, ] <- c(which(names(matrix_B) == b_pct_cols[i]), 0, 100)
}

# ppp and index variables as anchors
amelia_B <- amelia(
  x = as.data.frame(matrix_B),
  m = 1, # uses a single deterministic pass
  cs = "country",
  ts = "year",
  bounds = bounds_B_matrix,
  polytime = 1
)
block_B_clean <- amelia_B$imputations$imp1 %>%
  select(country, year, all_of(names(block_B)[-c(1,2)]))

# builds matrix_C and run the structural zero mask safely
matrix_C <- block_A_clean %>%
  inner_join(block_B_clean, by = c("country", "year")) %>%
  inner_join(block_C, by = c("country", "year")) %>%
  inner_join(select(energy_base, country, year, region), by = c("country", "year")) %>%
  mutate(region = as.factor(region))

# fuel columns
fuel_cols <- c("biogas_consumed_tj", "geothermal_energy_consumed_tj", 
               "hydro_energy_consumed_tj", "liquid_biofuels_consumed_tj", 
               "marine_consumed_tj", "modern_biomass_consumed_tj", 
               "solar_energy_consumed_tj", "traditional_biomass_consumed_tj", 
               "waste_energy_consumed_tj", "wind_energy_consumed_tj", 
               "primary_energy_savings_tj")

# dynamically create lag, lead, and mean columns
matrix_C_panel <- matrix_C %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    # complete panel timeline imputation using zoo
    across(all_of(fuel_cols), ~ {
      if(sum(!is.na(.x)) >= 2) {
        # interpolate internal missing years safely
        interp <- zoo::na.approx(.x, na.rm = FALSE)
        # extrapolate leading/trailing edge missing years to stabilize boundaries
        zoo::na.locf(zoo::na.locf(interp, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE)
      } else if (sum(!is.na(.x)) == 1) {
        # if only one historical point exists, carry it across the whole country timeline
        zoo::na.locf(zoo::na.locf(.x, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE)
      } else {
        .x
      }
    }),
    
    # Creates "lag_1_" prefixes for all fuels
    across(all_of(fuel_cols), ~ dplyr::lag(.x, 1), .names = "lag_1_{.col}"),
    
    # Creates "lead_1_" prefixes for all fuels
    across(all_of(fuel_cols), ~ dplyr::lead(.x, 1), .names = "lead_1_{.col}"),
    
    # Creates "mean_" prefixes for all country-specific baseline averages
    across(all_of(fuel_cols), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}")
  ) %>%
  ungroup() %>%
  mutate(
    country = as.factor(country),
    region = as.factor(region)
  )

# initialize predictor matrix
init_mice <- mice(matrix_C_panel, maxit = 0)
meth <- init_mice$method
pred <- init_mice$predictorMatrix

# adding CART method
for (col in names(matrix_C_panel)) {
  if (any(is.na(matrix_C_panel[[col]]))) {
    meth[col] <- "cart"  # tree methods prevent matrix algebra singularity i.e. determinant of zero
  } else {
    meth[col] <- ""      
  }
}

# refine predictor matrix
pred[, "country"] <- 0  # block country factor

# predicted by time, their own lags, leads, and country means
for (col in fuel_cols) {
  if (col %in% colnames(pred)) {
    # structural variables are removed
    pred[col, ] <- 0
    
    # strict timeline constraints for this variable
    pred[col, "year"] <- 1
    pred[col, paste0("lag_1_", col)] <- 1
    pred[col, paste0("lead_1_", col)] <- 1
    pred[col, paste0("mean_", col)] <- 1
  }
}

# run MICE with the full dynamic temporal architecture
mice_C <- mice(
  data = matrix_C_panel,
  m = 5,
  method = meth,
  predictorMatrix = pred,
  maxit = 10,
  seed = 123
)

# drop temporary anchor columns seamlessly
block_C_clean_mice <- complete(mice_C, 1) %>%
  select(-starts_with("lag_1_"), -starts_with("lead_1_"), -starts_with("mean_"))

# final dataset
final_energy_data <- energy_base %>%
  inner_join(block_C_clean_mice, by = c("country", "year"))

# verify completeness
vis_miss(final_energy_data) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0, size = 6))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# combine observed and imputed datasets for visualization
all_imputations <- complete(mice_C, "long", include = TRUE)

# test country for missing values
country_check <- all_imputations %>%
    filter(country == "Chile") %>%
    mutate(Data_Type = ifelse(.imp == 0, "Observed", paste0("Imputation ", .imp)))

# time-series trends
ggplot(country_check, aes(x = year, y = solar_energy_consumed_tj, color = Data_Type, group = .imp)) +
    geom_line(aes(alpha = ifelse(.imp == 0, 1, 0.4)), size = 1) +
    geom_point(aes(size = ifelse(.imp == 0, 2, 1))) +
    scale_alpha_identity() +
    scale_size_identity() +
    theme_minimal() +
    labs(title = "Time-Series Imputation Trajectory Check", color = "Dataset Type", x = "Year", y = "Solar Energy Consumed (TJ")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
stripplot(
  mice_C, 
  solar_energy_consumed_tj ~ .imp, 
  pch = 20, 
  cex = 0.5, 
  col = c("blue", "red"),
  main = "Observed vs. Imputed Solar Energy Consumption Data",
  xlab = "Imputation Number (1 = Observed Data)",
  ylab = "Solar Energy Consumed (TJ)",
  key = list(
    space = "right",
    text = list(c("Observed", "Imputed")),
    points = list(col = c("blue", "red"), pch = 20, cex = 1.0)
  )
)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
xyplot(
  mice_C, 
  solar_energy_consumed_tj ~ total_access_elec, 
  pch = 20, 
  cex = 0.6, 
  col = c("blue", "red"),
  main = "Cross-Variable Integrity: Solar Consumption vs. Electricity Access",
  xlab = "Total Electricity Access (%)",
  ylab = "Solar Energy Consumed (TJ)",
  key = list(
    space = "right",
    text = list(c("Observed", "Imputed")),
    points = list(col = c("blue", "red"), pch = 20, cex = 1.0)
  )
)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# parsing of imputation list
completed_datasets_long <- complete(mice_C, "long", include = TRUE)
mids_panel_wrapper <- as.mids(completed_datasets_long)
implist <- mice::complete(mids_panel_wrapper, action = "all")

panel_fits_q1 <- lapply(implist, function(d) {
  d_clean <- d %>%
    mutate(
      modern_renewable_tj = solar_energy_consumed_tj + wind_energy_consumed_tj +
        hydro_energy_consumed_tj + biogas_consumed_tj + 
        geothermal_energy_consumed_tj
    )
  
  # impact on traditional biomass
  fit_biomass <- plm(traditional_biomass_consumed_tj ~ total_access_elec, 
                     data = d_clean, index = c("country", "year"), model = "within", effect = "twoways")
  fit_biomass$vcov <- vcovHC(fit_biomass, type = "HC1", cluster = "group")
  
  # impact on modern renewables
  fit_modern <- plm(modern_renewable_tj ~ total_access_elec, 
                    data = d_clean, index = c("country", "year"), model = "within", effect = "twoways")
  fit_modern$vcov <- vcovHC(fit_modern, type = "HC1", cluster = "group")
  
  list(biomass = fit_biomass, modern = fit_modern)
})

fits_q1_biomass <- as.mira(lapply(panel_fits_q1, `[[`, "biomass"))
fits_q1_modern  <- as.mira(lapply(panel_fits_q1, `[[`, "modern"))

cat("\n Traditional Biomass Results \n")
print(summary(pool(fits_q1_biomass)))
cat("\n Modern Renewables Results \n")
print(summary(pool(fits_q1_modern)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
panel_fits_q2 <- lapply(implist, function(d) {
  # Dependent: absolute primary savings
  # Predictors: Economic structure (activity_index) vs Tech Efficiency (efficiency_index)
  model <- plm(
    primary_energy_savings_tj ~ activity_index + efficiency_index, 
    data = d, index = c("country", "year"), model = "within", effect = "twoways"
  )
  model$vcov <- vcovHC(model, type = "HC1", cluster = "group")
  return(model)
})

cat("\n Absolute Savings Drivers Results \n")
print(summary(pool(as.mira(panel_fits_q2))))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
regional_decoupling <- map_dfr(implist, function(d) {
  d %>%
    group_by(region, year) %>%
    summarise(
      regional_wealth = mean(activity_index, na.rm = TRUE),
      regional_energy = mean(total_primary_tj, na.rm = TRUE),
      .groups = "drop"
    )
}, .id = "imputation") %>%
  group_by(region, year) %>%
  # average imputations for stabilization of parameters
  summarise(
    wealth = mean(regional_wealth),
    energy = mean(regional_energy),
    .groups = "drop"
  ) %>%
  group_by(region) %>%
  # linear summary tracking for trends over time
  summarise(
    Wealth_Trend_Slope = coef(lm(wealth ~ year))[2],
    Energy_Consumption_Slope = coef(lm(energy ~ year))[2],
    .groups = "drop"
  ) %>%
  mutate(
    Decoupling_Status = case_when(
      Wealth_Trend_Slope > 0 & Energy_Consumption_Slope < 0 ~ "Absolute Decoupling (Ideal)",
      Wealth_Trend_Slope > 0 & Energy_Consumption_Slope > 0 ~ "Relative Decoupling / Coupled Growth",
      TRUE ~ "Stagnant / Regressive"
    )
  )

cat("\n--- Q3: REGIONAL ABSOLUTE DECOUPLING DIAGNOSTIC TABLE ---\n")
print(regional_decoupling)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
indexed_regional_data <- map_dfr(implist, function(d) {
  d %>%
    group_by(region, year) %>%
    summarise(
      regional_wealth = mean(activity_index, na.rm = TRUE),
      regional_energy = mean(total_primary_tj, na.rm = TRUE),
      .groups = "drop"
    )
}, .id = "imputation") %>%
  group_by(region, year) %>%
  summarise(
    Wealth = mean(regional_wealth),
    Energy = mean(regional_energy),
    .groups = "drop"
  ) %>%
  group_by(region) %>%
  arrange(year) %>%
  mutate(
    Wealth_Indexed = (Wealth / first(Wealth)) * 100,
    Energy_Indexed = (Energy / first(Energy)) * 100
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(Wealth_Indexed, Energy_Indexed),
    names_to = "Metric",
    values_to = "Indexed_Value"
  ) %>%
  mutate(
    Metric = ifelse(Metric == "Wealth_Indexed", "Economic Wealth (Index)", "Primary Energy Use (Index)")
  )

ggplot(indexed_regional_data, aes(x = year, y = Indexed_Value, color = Metric, linetype = Metric)) +
  geom_hline(yintercept = 100, color = "darkgray", linetype = "dashed", size = 0.5) +
  geom_line(size = 1.1) +
  geom_point(size = 1.5, alpha = 0.7) +
  facet_wrap(~ region, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("aquamarine4", "tomato2")) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "dimgray", size = 10, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 10, color = "black"),
    panel.background = element_rect(fill = "#FAF9F6", color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Testing for Absolute Decoupling Across Global Regions",
    subtitle = "Tracking cumulative growth relative to 1990 baseline levels (1990 = 100%)",
    x = "Year Timeline",
    y = "Indexed Growth Scale (%)"
  )


