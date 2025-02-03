# Time-Series Trends: Drought Severity Over Time (Enhanced)
# Key improvements: NA handling, accurate percentages, color scheme refinement

# 1. Load required packages
required_packages <- c("readr", "dplyr", "tidyr", "ggplot2", "lubridate")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 2. Data loading and validation
data_path <- file.path("data", "CA-drought-monitor-cat-percent-area.csv")

if (!file.exists(data_path)) {
  stop("Data file not found. Please verify path:\n", normalizePath(data_path))
}

# 3. Enhanced data processing
drought_data <- read_csv(data_path) %>%
  mutate(
    Week = as.Date(Week),
    Year = year(Week)
  ) %>%
  filter(!is.na(Year)) %>%  # Remove invalid dates
  pivot_longer(
    cols = matches("^D[0-4]$"),  # Strict column matching for D0-D4
    names_to = "drought_category",
    values_to = "percent_area"
  ) %>%
  filter(!is.na(percent_area)) %>%  # Remove missing values
  mutate(
    drought_category = factor(drought_category,
                              levels = c("D0", "D1", "D2", "D3", "D4"),
                              labels = c("Abnormally Dry", "Moderate Drought", 
                                         "Severe Drought", "Extreme Drought", "Exceptional Drought")
    )
  ) %>%
  group_by(Year, drought_category) %>%
  summarize(
    mean_percent = mean(percent_area, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Improved visualization
ggplot(drought_data, aes(x = Year, y = mean_percent, fill = drought_category)) +
  geom_area(position = "stack") +
  geom_vline(xintercept = c(2012, 2014), linetype = "dashed", alpha = 0.7) +
  scale_fill_manual(values = c(
    "#FFFFCC", "#FED976", "#FD8D3C", "#E31A1C", "#800026"
  )) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 2),
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 100)) +
  labs(
    title = "California Drought Severity Trends (2000-2023)",
    subtitle = "Annual Average of Land Area Affected by Drought Conditions",
    x = "Year",
    y = "Percentage of State Land Area",
    fill = "Drought Intensity",
    caption = "Source: U.S. Drought Monitor"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(margin = margin(b = 20)),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 11)
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 2,
    keywidth = unit(1.2, "cm"),
    keyheight = unit(0.6, "cm")
  ))