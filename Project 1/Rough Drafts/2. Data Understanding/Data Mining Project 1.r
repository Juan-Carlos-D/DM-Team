# =============================
# INSTALLATION OF PACKAGES (if needed)
# =============================

# install.packages("tidyverse")
# install.packages("ggrepel")
# install.packages("ggcorrplot")
# install.packages("DT")
# install.packages("gridExtra")
# install.packages("sf")
# install.packages("modeest")
# install.packages("factoextra")
# install.packages("kableExtra")
# install.packages("reshape2")
# install.packages("knitr")
# install.packages("caret")
# install.packages("car")

# =============================
# LOAD REQUIRED LIBRARIES
# =============================

library("tidyverse")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("sf")
library("modeest")
library("knitr")
library("factoextra")
library("reshape2")
library("kableExtra")
library("knitr")
library("stringr")
library("caret")
library("car")

# =============================
# DEFINE BASE DIRECTORY AND FILE PATHS
# =============================

base_dir <- "../../../../../../../Desktop/COVID-19/"
# base_dir <- "/Users/salissa/Desktop/Data Mining/Datasets/COVID-19"

global_mobility_path <- file.path(base_dir, "Global_Mobility_Report.csv")
covid_cases_census_path <- file.path(base_dir, "c19_census.csv")
covid_cases_tx_path <- file.path(base_dir, "c19_tx.csv")

# =============================
# FUNCTION TO READ CSV FILES
# =============================

read_data <- function(file_path, dataset_name) {
  if (file.exists(file_path)) {
    cat(paste0("\n--- Loading ", dataset_name, " ---\n"))
    return(read_csv(file_path, show_col_types = FALSE))
  } else {
    stop(paste0("Error: File not found -> ", file_path))
  }
}

# =============================
# LOAD DATASETS
# =============================

global_mobility <- read_data(global_mobility_path, "Global Mobility Data")
covid_cases_census <- read_data(covid_cases_census_path, "COVID-19 Cases and Census Data")
covid_cases_tx <- read_data(covid_cases_tx_path, "COVID-19 Cases for Texas")

cat("\n--- All datasets successfully loaded! ---\n")

# =============================
# RANDOMLY DISPLAY 10 ROWS FROM THE GLOBAL MOBILITY DATA
# =============================

global_mobility %>%
  sample_n(10)

# =============================
# VIEW STRUCTURE OF THE GLOBAL MOBILITY DATA
# =============================

glimpse(global_mobility)

# =============================
# RENAME COLUMNS AND FILTER RELEVANT VARIABLES
# =============================

global_mobility_filtered <- global_mobility %>%
  select(
    date,
    country = country_region,
    region_1 = sub_region_1,
    region_2 = sub_region_2,
    retail_change = retail_and_recreation_percent_change_from_baseline,
    grocery_change = grocery_and_pharmacy_percent_change_from_baseline,
    workplace_change = workplaces_percent_change_from_baseline,
    residential_change = residential_percent_change_from_baseline,
    transit_change = transit_stations_percent_change_from_baseline
  )

# =============================
# FILTER DATASET FOR TEXAS ONLY
# =============================

texas_mobility_data <- global_mobility_filtered %>%
  filter(country == "United States" & region_1 == "Texas")

# =============================
# CONVERT DATA TYPES
# =============================

texas_mobility_data <- texas_mobility_data %>%
  mutate(
    country = as.factor(country),
    region_1 = as.factor(region_1),
    region_2 = as.factor(region_2),
    date = as.Date(date)
  )

# =============================
# CHECK FOR MISSING VALUES IN EACH COLUMN
# =============================

missing_values <- texas_mobility_data %>%
  summarize(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count") %>%
  mutate(missing_pct = (missing_count / nrow(texas_mobility_data)) * 100)

print(missing_values)

# =============================
# REMOVE ROWS WITH MISSING VALUES
# =============================

texas_mobility_data <- texas_mobility_data %>%
  drop_na(region_2, retail_change, grocery_change, workplace_change, residential_change, transit_change)

cat("Remaining rows after missing value removal:", nrow(texas_mobility_data), "\n")

# =============================
# IDENTIFY AND REMOVE DUPLICATE ROWS
# =============================

duplicates <- texas_mobility_data %>%
  filter(duplicated(.))
num_duplicates <- nrow(duplicates)
cat("Number of duplicate rows:", num_duplicates, "\n")

if (num_duplicates > 0) {
  texas_mobility_data <- texas_mobility_data %>% distinct()
  cat("Duplicates removed. New dataset size:", nrow(texas_mobility_data), "rows\n")
} else {
  cat("No duplicate rows found.\n")
}

# =============================
# IDENTIFY OUTLIERS USING IQR
# =============================

count_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  sum(x < (Q1 - 1.5 * IQR_value) | x > (Q3 + 1.5 * IQR_value), na.rm = TRUE)
}

outlier_summary <- texas_mobility_data %>%
  summarize(across(
    c(retail_change, grocery_change, workplace_change, residential_change, transit_change),
    count_outliers,
    .names = "{.col}_outliers"
  )) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Outlier_Count")

print(outlier_summary, width = Inf)

# =============================
# REMOVE OUTLIERS FROM THE DATASET
# =============================

is_not_outlier <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  x >= (Q1 - 1.5 * IQR_value) & x <= (Q3 + 1.5 * IQR_value)
}

texas_mobility_data_cleaned <- texas_mobility_data %>%
  filter(
    is_not_outlier(retail_change) &
    is_not_outlier(grocery_change) &
    is_not_outlier(workplace_change) &
    is_not_outlier(residential_change) &
    is_not_outlier(transit_change)
  )

rows_removed <- nrow(texas_mobility_data) - nrow(texas_mobility_data_cleaned)
cat("Number of rows removed due to outliers:", rows_removed, "\n")

# =============================
# FINAL DATA SUMMARY AFTER CLEANING
# =============================

summary(texas_mobility_data_cleaned)

# ===========================================================
# Function to compute key statistics
# ===========================================================
compute_mobility_stats <- function(data, variables) {
  data %>%
    summarize(across(
      all_of(variables),
      list(
        Range = ~ max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE),
        Mode = ~ mfv(.x),
        Mean = ~ mean(.x, na.rm = TRUE),
        Median = ~ median(.x, na.rm = TRUE),
        Variance = ~ var(.x, na.rm = TRUE),
        SD = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ))
}

# ===========================================================
# Define mobility variables
# ===========================================================
mobility_variables <- c("retail_change", "grocery_change", "workplace_change", "residential_change", "transit_change")

# ===========================================================
# Compute statistics
# ===========================================================
mobility_stats_table <- compute_mobility_stats(texas_mobility_data_cleaned, mobility_variables)

# ===========================================================
# Convert to long format for better readability
# ===========================================================
mobility_stats_long <- mobility_stats_table %>%
  pivot_longer(cols = everything(),
               names_to = c("Variable", "Statistic"),
               names_pattern = "(.*)_(Range|Mode|Mean|Median|Variance|SD)",
               values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value)

# ===========================================================
# Print the table
# ===========================================================
print(mobility_stats_long, width=Inf)

# ===========================================================
# Display a static, nicely formatted table
# ===========================================================
kable(mobility_stats_long, caption = "Summary Statistics for Mobility Data")

# ===========================================================
# Retail and Recreation Percent Change
# ===========================================================
ggplot(texas_mobility_data_cleaned,
       aes(x = retail_change)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Retail & Recreation Mobility",
       x = "Percent Change",
       y = "Frequency")

# ===========================================================
# Grocery and Pharmacy Percent Change
# ===========================================================
ggplot(
  texas_mobility_data_cleaned,
  aes(x = grocery_change)
) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  theme_minimal() +
  labs(
    title = "Grocery & Pharmacy Mobility",
    x = "Percent Change",
    y = "Frequency"
  )

# ===========================================================
# Workplace Mobility Histogram
# ===========================================================
ggplot(texas_mobility_data, aes(x = workplace_change)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Workplace Mobility", x = "Percent Change", y = "Frequency")

# ===========================================================
# Residential Mobility Histogram
# ===========================================================
ggplot(texas_mobility_data, aes(x = residential_change)) +
  geom_histogram(binwidth = 2, fill = "purple", color = "black") +
  theme_minimal() +
  labs(title = "Residential Mobility", x = "Percent Change", y = "Frequency")

# ===========================================================
# Transit Station Mobility Histogram
# ===========================================================
ggplot(
  texas_mobility_data,
  aes(x = transit_change)
) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  theme_minimal() +
  labs(
    title = "Transit Station Mobility",
    x = "Percent Change",
    y = "Frequency"
  )

# ===========================================================
# Selecting relevant columns
# ===========================================================
mobility_vars <- texas_mobility_data_cleaned %>%
  select(retail_change,
         grocery_change,
         workplace_change,
         residential_change,
         transit_change)

# ===========================================================
# Compute correlation matrix
# ===========================================================
cor_matrix <- cor(mobility_vars, use = "complete.obs")

# ===========================================================
# Plot heatmap
# ===========================================================
ggcorrplot(cor_matrix,
  method = "square", type = "lower", lab = TRUE,
  colors = c("blue", "white", "red"),
  title = "Correlation Matrix of Mobility Changes"
)

# ===========================================================
# Compute county-level averages for retail and workplace mobility
# ===========================================================
county_mobility <- texas_mobility_data_cleaned %>%
  group_by(region_2) %>%
  summarize(
    avg_retail = mean(retail_change, na.rm = TRUE),
    avg_workplace = mean(workplace_change, na.rm = TRUE)
  ) %>%
  arrange(avg_retail)

# ===========================================================
# View first few rows
# ===========================================================
head(county_mobility)

# ===========================================================
# Top 15 hardest-hit counties 
# ===========================================================
top_counties <- county_mobility %>%
  slice_min(avg_retail, n = 15)

# ===========================================================
# Bar plot retail mobility
# ===========================================================
ggplot(top_counties, aes(x = reorder(region_2, avg_retail), y = avg_retail)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +  
  coord_flip() +
  theme_minimal(base_size = 14) + 
  labs(title = "Top 15 Counties w/ Largest Retail Mobility Decline",
       x = "County", y = "Average Percent Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),  
        axis.text.y = element_text(size = 12),  
        axis.text.x = element_text(size = 10)) 

# ===========================================================
# Bar plot workplace mobility 
# ===========================================================
top_counties <- county_mobility %>%
  slice_min(avg_workplace, n = 15)

ggplot(top_counties, aes(x = reorder(region_2, avg_workplace), y = avg_workplace)) +
  geom_col(fill = "firebrick", alpha = 0.8, width = 0.7) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Top 15 Counties w/ Largest Workplace Mobility Decline",
       x = "County", y = "Average Percent Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10))

# ===========================================================
# Aggregate/week for smoother trends
# ===========================================================
mobility_trends <- texas_mobility_data_cleaned %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarize(
    avg_grocery = mean(grocery_change, na.rm = TRUE),
    avg_retail = mean(retail_change, na.rm = TRUE)
  )

# ===========================================================
# Improved line plot with weekly data
# ===========================================================
ggplot(mobility_trends, aes(x = week)) +
  geom_line(aes(y = avg_grocery, color = "Grocery & Pharmacy"), linewidth = 1) +
  geom_line(aes(y = avg_retail, color = "Retail & Recreation"), linewidth = 1, linetype = "dashed") +
  theme_minimal(base_size = 14) +  
  scale_color_manual(values = c("Grocery & Pharmacy" = "green", "Retail & Recreation" = "blue")) +  
  labs(title = "Comparison of Grocery & Pharmacy vs. Retail & Recreation Mobility",  
       x = "Date", y = "Percent Change from Baseline", color = "Mobility Type") +  
  theme(legend.position = "bottom",
        legend.margin = margin(10, 10, 10, 10),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# ===========================================================
# Filtering data for major metropolitan areas and rural areas
# ===========================================================
metro_areas <- c("Harris County", "Dallas County", "Travis County", "Bexar County", "Tarrant County", "Collin County", "Denton County") 
texas_mobility_data_cleaned$metro_area <- ifelse(texas_mobility_data_cleaned$region_2 %in% metro_areas, 
                                                 "Metro", "Rural")
# As factor
texas_mobility_data_cleaned$metro_area <- as.factor(texas_mobility_data_cleaned$metro_area)

# ===========================================================
# Scatter plot for transit stations
# ===========================================================
ggplot(texas_mobility_data_cleaned, aes(x = date, y = transit_change, 
                                       color = metro_area)) +
  geom_point(alpha = 0.6, size = 1) +
  labs(title = "Transit Stations Mobility Trend", x = "Date", 
       y = "Percent Change from Baseline",
       color = "Area Type") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Metro" = "blue", "Rural" = "red")) +
  theme(legend.position = "bottom")

# ===========================================================
# Group data by metro area and summarize mobility trends
# ===========================================================
summary_stats <- texas_mobility_data_cleaned %>%
  group_by(metro_area) %>%
  summarize(
    min_change = min(transit_change, na.rm = TRUE),
    avg_change = mean(transit_change, na.rm = TRUE),
    latest_change = transit_change[which.max(date)]
  )

print(summary_stats)

# ===========================================================
# Visual of trends
# ===========================================================
ggplot(texas_mobility_data_cleaned, 
       aes(x = as.Date(date), y = transit_change, color = metro_area)) +
  geom_line(stat = "smooth", formula = y ~ x, method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Transit Mobility Trends Over Time", 
       x = "Date", 
       y = "Percent Change from Baseline") +
  scale_color_manual(values = c("Metro" = "blue", "Rural" = "red")) +
  theme_minimal()

# ===========================================================
# Scatter plot for workplaces
# ===========================================================
ggplot(texas_mobility_data_cleaned, aes(x = as.Date(date), y = workplace_change, 
                                       color = metro_area)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(data = subset(texas_mobility_data_cleaned, metro_area == "Metro"),
              method = "loess", formula = y ~ x, se = FALSE, linetype = "solid", linewidth = 1.2, color = "#1E90FF") + 
  geom_smooth(data = subset(texas_mobility_data_cleaned, metro_area == "Rural"),
              method = "loess", formula = y ~ x, se = FALSE, linetype = "dashed", linewidth = 1.2, color = "#8A2BE2") +
  labs(
    title = "Workplaces Mobility Trend", x = "Date",
    y = "Percent Change from Baseline"
  ) +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = c("Metro" = "darkgreen", "Rural" = "darkgoldenrod2")) +
    theme(legend.position = "bottom")
  
# ===========================================================
# Summary stats workplace mobility
# ===========================================================
summary_stats <- texas_mobility_data_cleaned %>%
  group_by(metro_area) %>%
  summarize(
    min_change = min(workplace_change, na.rm = TRUE),
    avg_change = mean(workplace_change, na.rm = TRUE),
    latest_change = workplace_change[which.max(date)]
  )

print(summary_stats)

# ===========================================================
# Visual of trends
# ===========================================================
ggplot(texas_mobility_data_cleaned, aes(x = as.Date(date), y = workplace_change, color = metro_area)) +
  geom_line(stat = "smooth", formula = y ~ x, method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Workplace Mobility Trends Over Time", x = "Date", y = "Percent Change from Baseline") +
  scale_color_manual(values = c("Metro" = "blue", "Rural" = "red")) +
  theme_minimal()

# ===========================================================
# Scatter plot for residential mobility
# ===========================================================
ggplot(
  texas_mobility_data_cleaned,
  aes(x = as.Date(date), y = residential_change, color = metro_area)
) +
  geom_point(alpha = 0.4, size = 1) +
  labs(
    title = "Residential Mobility Trend",
    x = "Date",
    y = "Percent Change from Baseline",
    color = "Area Type"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Metro" = "blue1", "Rural" = "#ec1010"))

# ===========================================================
# Summary stats residential
# ===========================================================
summary_stats <- texas_mobility_data_cleaned %>%
  group_by(metro_area) %>%
  summarize(
    min_change = min(residential_change, na.rm = TRUE),
    avg_change = mean(residential_change, na.rm = TRUE),
    latest_change = residential_change[which.max(date)]
  )

print(summary_stats)

# ===========================================================
# Visual of trends
# ===========================================================
ggplot(texas_mobility_data_cleaned, aes(x = as.Date(date), y = residential_change, color = metro_area)) +
  geom_line(stat = "smooth", formula = y ~ x, method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Workplace Mobility Trends Over Time", x = "Date", y = "Percent Change from Baseline") +
  scale_color_manual(values = c("Metro" = "blue", "Rural" = "red")) +
  theme_minimal()

# ================================================================
# Prepare data for PCA
# ================================================================
mobility_features <- texas_mobility_data_cleaned %>%
  select(
    retail_change,
    grocery_change,
    workplace_change,
    residential_change,
    transit_change
  )

# ================================================================
# Standardize data for PCA
# ================================================================
mobility_data_scaled <- scale(mobility_features)

# ================================================================
# Perform PCA
# ================================================================
pca <- prcomp(mobility_data_scaled, center = TRUE, scale. = TRUE)

# ================================================================
# PCA Summary
# ================================================================
pca_summary <- summary(pca)
print(pca_summary)

# ================================================================
# View PCA loadings (eigenvectors)
# ================================================================
print(pca$rotation)

# ================================================================
# Scree plot to visualize variance
# ================================================================
screeplot(pca, type = "lines", main = "Scree Plot of PCA")

# ================================================================
# Summary stats for PCA
# ================================================================
summary_stats_pca <- data.frame(
  PC = paste0("PC", 1:length(pca$sdev)),  
  Standard_Deviation = pca$sdev,  
  Variance_Explained = (pca$sdev^2) / sum(pca$sdev^2),  
  Cumulative_Variance = cumsum((pca$sdev^2) / sum(pca$sdev^2))  
)
# View PCA stats
print(summary_stats_pca)  

# ================================================================
# Extract PCA loadings
# ================================================================
pca_loadings <- as.data.frame(pca$rotation)
pca_loadings$Variable <- rownames(pca_loadings)
rownames(pca_loadings) <- NULL  

# ================================================================
# Extract PCA scores & county information
# ================================================================
biplot_data <- as.data.frame(pca$x) %>%
  mutate(County = texas_mobility_data_cleaned$region_2)

# ================================================================
# Scree Plot Data
# ================================================================
scree_data <- data.frame(
  PC = 1:length(pca$sdev),
  Variance = (pca$sdev^2) / sum(pca$sdev^2)
)

# ================================================================
# Ensure PC is a factor for the Scree plot
# ================================================================
scree_data$PC <- factor(scree_data$PC)

# ================================================================
# Visualize Scree Plot
# ================================================================
ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.6) +
  geom_text(aes(label = round(Variance, 3)), vjust = -0.5, size = 4) + 
  labs(title = "Scree Plot", x = "Principal Component", y = "Variance Explained") +
  theme_minimal()

# ================================================================
# PCA Plot (First Two Principal Components)
# ================================================================
# Adjust plot size and improve visibility
pca_plot <- ggplot(biplot_data, aes(x = PC1, y = PC2, color = County)) +
  geom_point(alpha = 0.7, size = 3) + 
  labs(
    title = "PCA: Mobility Data Dimensionality Reduction",
    x = "Principal Component 1 (PC1)", 
    y = "Principal Component 2 (PC2)",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  
    legend.key.size = unit(0.4, "cm"),  
    legend.text = element_text(size = 10), 
    plot.margin = margin(15, 15, 15, 15) 
  ) +
  guides(color = guide_legend(ncol = 5))  

# ================================================================
# Increase figure size
# ================================================================
options(repr.plot.width = 15, repr.plot.height = 10)

# ================================================================
# Display the plot
# ================================================================
print(pca_plot)

# ================================================================
# Save the plot with adjusted dimensions
# ================================================================
ggsave("pca_plot.png", plot = pca_plot, width = 12, height = 10, dpi = 300)

# ================================================================
# Convert wide to long format for PCA loadings
# ================================================================
loading_df_long <- pca_loadings %>%
  pivot_longer(cols = -Variable, names_to = "PC", values_to = "loading_value")

# ================================================================
# Create PCA Loadings Plot
# ================================================================
ggplot(loading_df_long, aes(x = Variable, y = loading_value, fill = PC)) +
  geom_bar(stat = "identity", position = "dodge") +  
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal() +
  theme(
    text = element_text(size = 12),  
    legend.position = "bottom",  
    axis.text.y = element_text(size = 10)  
  ) +
  labs(
    title = "PCA Loadings: Contribution of Each Variable to Principal Components",
    subtitle = "Higher absolute values indicate stronger influence on the principal component",
    x = "Variables", 
    y = "Loading Value",
    fill = "Principal Component"
  )

# ================================================================
# Prints the PCA loadings
# ================================================================
print(pca$rotation, 4)

# ================================================================
# Adjust plot size for the biplot
# ================================================================
options(repr.plot.width = 16, repr.plot.height = 14)

# ================================================================
# PCA Biplot PC1 vs PC2
# ================================================================
ggplot(biplot_data, aes(x = PC1, y = PC2, color = County)) +
  geom_point(alpha = 0.7, size = 3) +  
  geom_segment(data = pca_loadings, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), 
               color = "blue", linewidth = 1.2) +  
  geom_text_repel(data = pca_loadings, 
                  aes(x = PC1 * 1.5, y = PC2 * 1.5, label = Variable), 
                  color = "black", size = 6, fontface = "bold", 
                  box.padding = 0.5, 
                  point.padding = 0.5, 
                  max.overlaps = 10) +
  labs(
    title = "PCA Biplot (PC1 vs. PC2)",
    subtitle = "Projection of Variables and Counties onto the Principal Components",
    x = "Principal Component 1 (PC1)", 
    y = "Principal Component 2 (PC2)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    legend.text = element_text(size = 10), 
    axis.text = element_text(size = 12), 
    plot.title = element_text(size = 16, face = "bold"),  
    plot.subtitle = element_text(size = 12)  
  ) +
  coord_fixed(ratio = 1)

# ================================================================
# Extract PCA Loadings for PC1 and PC2
# ================================================================
pca_loadings_pc1_pc2 <- as.data.frame(round(pca$rotation[, 1:2], 4))  
pca_loadings_pc1_pc2$Variable <- rownames(pca_loadings_pc1_pc2)  
rownames(pca_loadings_pc1_pc2) <- NULL  

# ================================================================
# Print PCA Loadings Summary for PC1 and PC2
# ================================================================
print("PCA Loadings for PC1 and PC2:")
print(pca_loadings_pc1_pc2)

# ================================================================
# Extract variance explained by PC1 and PC2
# ================================================================
explained_variance_pc1_pc2 <- round(sum(pca$sdev[1:2]^2) / sum(pca$sdev^2) * 100, 2)

# ================================================================
# Print explained variance for PC1 and PC2
# ================================================================
print(paste("PC1 and PC2 together explain", explained_variance_pc1_pc2, "% of the total variance."))

# ================================================================
# Reduce plot size
# ================================================================
options(repr.plot.width = 10, repr.plot.height = 6)

# ================================================================
# Scree Plot Data
# ================================================================
scree_data <- data.frame(PC = 1:length(pca$sdev), Variance = pca$sdev^2 / sum(pca$sdev^2))

# ================================================================
# Ensure PC is a factor for the Scree plot
# ================================================================
scree_data$PC <- factor(scree_data$PC)

# ================================================================
# Highlight PC1 & PC2 in the Scree Plot
# ================================================================
ggplot(scree_data, aes(x = PC, y = Variance, fill = (PC %in% c("1", "2")))) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Variance, 3)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("gray", "skyblue"), guide = "none") + 
  labs(title = "Scree Plot (PC1 & PC2 Highlighted)", 
       x = "Principal Component", y = "Variance Explained") +
  theme_minimal()
