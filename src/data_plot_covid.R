

# Load necessary libraries
library(ggplot2)      # For creating visualizations
library(dplyr)        # For data manipulation
library(tidyr)        # For reshaping data
library(scales)       # For formatting axes
library(RColorBrewer) # For color palettes
library(treemap)      # For treemap visualization
library(ggrepel)      # For non-overlapping text labels
library(treemapify)   # For resolving error treemap renderer

# Read the CSV file
# Replace with your file path
covid_data <- read.csv("./data/DATA_REKAP_COVID19.csv", stringsAsFactors = FALSE)

# Display structure of the data
str(covid_data)
summary(covid_data)

# Basic information about the data
cat("Number of regions:", nrow(covid_data), "\n")
cat("Time period:", unique(covid_data$periode_data), "\n")
cat("Provinces:", unique(covid_data$nama_provinsi), "\n")

# 1. VISUALIZATION: Case Overview by District (Kecamatan)
# Create a summary by district
district_summary <- covid_data %>%
  group_by(nama_kota, nama_kecamatan) %>%
  summarize(
    total_odp = sum(odp),
    total_pdp = sum(pdp),
    total_positif = sum(positif),
    total_meninggal = sum(meninggal),
    total_sembuh = sum(sembuh),
    .groups = 'drop'
  )

# Create a bar chart for case overview
ggplot(district_summary, aes(x = reorder(nama_kecamatan, -total_positif), y = total_positif, fill = nama_kota)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_positif), vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "COVID-19 Positive Cases by District in Jakarta",
    subtitle = "July 2020",
    x = "District (Kecamatan)",
    y = "Number of Positive Cases",
    fill = "City"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# 2. VISUALIZATION: Case Status Distribution
# Reshape data for visualization
status_data <- covid_data %>%
  summarize(
    Active = sum(dirawat),
    Recovered = sum(sembuh),
    Deaths = sum(meninggal),
    `Self Isolation` = sum(self_isolation),
    `Under Observation` = sum(odp),
    `Under Monitoring` = sum(pdp)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Status", values_to = "Count")

# Create a pie chart
ggplot(status_data, aes(x = "", y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Distribution of COVID-19 Cases by Status",
    subtitle = "Jakarta, July 2020",
    fill = "Status"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  geom_text(aes(label = paste0(round(Count/sum(Count)*100, 1), "%")), 
            position = position_stack(vjust = 0.5))

# 3. VISUALIZATION: Comparison of Key Metrics
# Reshape the data
metrics_by_region <- covid_data %>%
  select(nama_kelurahan, positif, sembuh, meninggal, self_isolation) %>%
  pivot_longer(
    cols = c(positif, sembuh, meninggal, self_isolation),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = factor(metric, 
                         levels = c("positif", "sembuh", "meninggal", "self_isolation"),
                         labels = c("Positive", "Recovered", "Deaths", "Self Isolation")))

# Create a grouped bar chart
ggplot(metrics_by_region, aes(x = nama_kelurahan, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "COVID-19 Metrics by Sub-district (Kelurahan)",
    subtitle = "Jakarta, July 2020",
    x = "Sub-district",
    y = "Count",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# 4. VISUALIZATION: ODP vs PDP Scatter Plot
ggplot(covid_data, aes(x = odp, y = pdp, size = positif, color = nama_kota)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(aes(label = nama_kelurahan), size = 3, max.overlaps = 5) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Relationship Between People Under Observation (ODP) and Patients Under Supervision (PDP)",
    subtitle = "Size indicates number of positive cases",
    x = "Number of People Under Observation (ODP)",
    y = "Number of Patients Under Supervision (PDP)",
    size = "Positive Cases",
    color = "City"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# 5. VISUALIZATION: Monitoring Status
monitoring_data <- covid_data %>%
  select(nama_kelurahan, proses_pemantauan, selesai_pemantauan, odp_meninggal) %>%
  pivot_longer(
    cols = c(proses_pemantauan, selesai_pemantauan, odp_meninggal),
    names_to = "status",
    values_to = "count"
  ) %>%
  mutate(status = factor(status, 
                         levels = c("proses_pemantauan", "selesai_pemantauan", "odp_meninggal"),
                         labels = c("In Process", "Completed", "Died During Observation")))

ggplot(monitoring_data, aes(x = nama_kelurahan, y = count, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("In Process" = "#66c2a5", 
                               "Completed" = "#fc8d62", 
                               "Died During Observation" = "#8da0cb")) +
  labs(
    title = "Monitoring Status of People Under Observation (ODP)",
    subtitle = "Jakarta, July 2020",
    x = "Sub-district",
    y = "Number of People",
    fill = "Monitoring Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Alternative to treemap using ggplot2
treemap_data <- covid_data %>%
  group_by(nama_kota, nama_kecamatan) %>%
  summarize(
    total_deaths = sum(meninggal),
    .groups = 'drop'
  )

# Create visualization of deaths by region using standard ggplot2
ggplot(treemap_data, aes(x = reorder(nama_kecamatan, -total_deaths), y = total_deaths, fill = nama_kota)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_deaths), vjust = -0.5) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "COVID-19 Deaths by District",
    x = "District",
    y = "Total Deaths",
    fill = "City"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. VISUALIZATION: Recovery vs Mortality Ratio
ratio_data <- covid_data %>%
  group_by(nama_kecamatan) %>%
  summarize(
    total_positif = sum(positif),
    total_sembuh = sum(sembuh),
    total_meninggal = sum(meninggal),
    recovery_rate = ifelse(total_positif > 0, total_sembuh / total_positif * 100, 0),
    mortality_rate = ifelse(total_positif > 0, total_meninggal / total_positif * 100, 0),
    .groups = 'drop'
  )

# Create a scatter plot for recovery vs mortality rates
ggplot(ratio_data, aes(x = recovery_rate, y = mortality_rate, size = total_positif)) +
  geom_point(aes(color = nama_kecamatan), alpha = 0.7) +
  geom_text_repel(aes(label = nama_kecamatan), size = 3) +
  scale_size_continuous(range = c(3, 10)) +
  labs(
    title = "Recovery Rate vs Mortality Rate by District",
    subtitle = "Size indicates number of positive cases",
    x = "Recovery Rate (%)",
    y = "Mortality Rate (%)",
    size = "Total Positive Cases",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# 8. VISUALIZATION: Case Severity Comparison
severity_data <- covid_data %>%
  mutate(
    severity_ratio = ifelse(pdp > 0, meninggal / pdp, 0),
    isolation_ratio = ifelse(positif > 0, self_isolation / positif, 0)
  )

ggplot(severity_data, aes(x = nama_kelurahan, y = severity_ratio, fill = nama_kota)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Case Severity by Sub-district",
    subtitle = "Measured by Deaths to PDP ratio",
    x = "Sub-district",
    y = "Severity Ratio (Deaths / PDP)",
    fill = "City"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# 9. VISUALIZATION: Stacked area chart of case composition
# Prepare data for stacked area chart (hypothetical time series)
# Since we only have one time period, we'll create a hypothetical progression
# This is a demonstration of how you would visualize time series data if available
dates <- seq(as.Date("2020-07-01"), as.Date("2020-07-31"), by = "day")
areas <- unique(covid_data$nama_kecamatan)

# Create a time series dataframe
create_time_series <- function(area_data, area_name) {
  start_cases <- area_data$positif[1]
  end_cases <- start_cases * (1 + runif(1, 0.2, 0.5))  # Simulate 20-50% increase
  case_progression <- seq(start_cases, end_cases, length.out = length(dates))
  
  start_deaths <- area_data$meninggal[1]
  end_deaths <- start_deaths * (1 + runif(1, 0.1, 0.3))  # Simulate 10-30% increase
  death_progression <- seq(start_deaths, end_deaths, length.out = length(dates))
  
  start_recovered <- area_data$sembuh[1]
  end_recovered <- start_recovered * (1 + runif(1, 0.3, 0.6))  # Simulate 30-60% increase
  recovered_progression <- seq(start_recovered, end_recovered, length.out = length(dates))
  
  data.frame(
    date = dates,
    area = rep(area_name, length(dates)),
    cases = round(case_progression),
    deaths = round(death_progression),
    recovered = round(recovered_progression)
  )
}

# Generate time series data for each area
time_series_data <- data.frame()
for(area in areas) {
  area_data <- covid_data %>% filter(nama_kecamatan == area)
  if(nrow(area_data) > 0) {
    area_time_series <- create_time_series(area_data, area)
    time_series_data <- rbind(time_series_data, area_time_series)
  }
}

# Plot the time series (stacked area chart)
ggplot(time_series_data, aes(x = date, y = cases, fill = area)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Simulated COVID-19 Case Progression by District",
    subtitle = "Note: This is a simulated projection based on July 2020 data",
    x = "Date",
    y = "Cumulative Cases",
    fill = "District"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# 10. VISUALIZATION: Heatmap of case density
# Prepare data for heatmap
# We'll use the actual counts as a proxy for density
heatmap_data <- covid_data %>%
  group_by(nama_kota, nama_kecamatan) %>%
  summarize(
    odp_density = sum(odp),
    pdp_density = sum(pdp),
    positif_density = sum(positif),
    .groups = 'drop'
  )

# Create the heatmap
ggplot(heatmap_data, aes(x = nama_kota, y = nama_kecamatan)) +
  geom_tile(aes(fill = positif_density), color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_text(aes(label = positif_density), color = "black", size = 3) +
  labs(
    title = "Heatmap of COVID-19 Positive Cases by District and City",
    x = "City",
    y = "District",
    fill = "Positive Cases"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Export plots (optional)
# You can save any of these plots to a file using ggsave()
# For example:
# ggsave("covid_cases_by_district.png", width = 10, height = 6, dpi = 300)