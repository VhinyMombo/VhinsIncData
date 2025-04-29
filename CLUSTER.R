# Country Clustering Analysis based on World Bank and IMF Indicators
# This script performs clustering of countries based on economic and development indicators

# Install required packages if not already installed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("cluster")) install.packages("cluster")
if (!require("factoextra")) install.packages("factoextra")
if (!require("countrycode")) install.packages("countrycode")
if (!require("WDI")) install.packages("WDI")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("opensdmx/rsdmx", force = T)
# Load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(countrycode)
library(WDI)
library(rsdmx)

# library(imfr)

# Set seed for reproducibility
set.seed(123)

# Step 1: Data Collection
# -----------------------

# A. World Bank indicators
# Define indicators to download
wb_indicators <- c(
  # Economic indicators
  "NY.GDP.PCAP.PP.KD",     # GDP per capita, PPP (constant 2017 international $)
  "NY.GDP.MKTP.KD.ZG",     # GDP growth (annual %)
  "NE.TRD.GNFS.ZS",        # Trade (% of GDP)
  "BX.KLT.DINV.WD.GD.ZS",  # Foreign direct investment, net inflows (% of GDP)
  "FP.CPI.TOTL.ZG",        # Inflation, consumer prices (annual %)
  "GC.TAX.TOTL.GD.ZS",     # Tax revenue (% of GDP)
  # Financial indicators
  "GC.DOD.TOTL.GD.ZS",     # Central government debt, total (% of GDP)
  "BN.CAB.XOKA.GD.ZS",     # Current account balance (% of GDP)
  "CM.MKT.LCAP.GD.ZS",     # Stock market capitalization (% of GDP)
  "FS.AST.PRVT.GD.ZS",     # Domestic credit to private sector (% of GDP)
  # Labor market and inequality
  "SL.UEM.TOTL.ZS",        # Unemployment, total (% of total labor force)
  "SL.TLF.CACT.FE.ZS",     # Labor force participation rate, female (% of female population ages 15+)
  "SI.POV.GINI",           # GINI index (World Bank estimate)
  "SI.POV.NAHC",           # Poverty headcount ratio at national poverty lines (% of population)
  # Health indicators
  "SP.DYN.LE00.IN",        # Life expectancy at birth, total (years)
  "SH.XPD.CHEX.GD.ZS",     # Current health expenditure (% of GDP)
  "SH.IMM.MEAS",           # Immunization, measles (% of children ages 12-23 months)
  "SH.STA.MMRT",           # Maternal mortality ratio (per 100,000 live births)
  # Education indicators
  "SE.XPD.TOTL.GD.ZS",     # Government expenditure on education, total (% of GDP)
  "SE.TER.ENRR",           # School enrollment, tertiary (% gross)
  "SE.ADT.LITR.ZS",        # Literacy rate, adult total (% of people ages 15 and above)
  # Infrastructure and technology
  "IT.NET.USER.ZS",        # Individuals using the Internet (% of population)
  "IT.CEL.SETS.P2",        # Mobile cellular subscriptions (per 100 people)
  "EG.USE.ELEC.KH.PC",     # Electric power consumption (kWh per capita)
  # Environmental indicators
  "EN.ATM.CO2E.PC",        # CO2 emissions (metric tons per capita)
  "ER.PTD.TOTL.ZS",        # Terrestrial and marine protected areas (% of total territorial area)
  "AG.LND.FRST.ZS",        # Forest area (% of land area)
  # Governance indicators
  "IQ.CPA.PROT.XQ",        # CPIA property rights and rule-based governance rating
  "IC.BUS.EASE.XQ"         # Ease of doing business score
)

# Download World Bank data (most recent 5 years)
wb_data <- WDI(indicator = wb_indicators, 
               start = 2000, end = 2022, 
               extra = TRUE) |> data.table()

# Clean WB data
wb_data_clean <- wb_data[!is.na(iso3c), country, iso3c, year, 
                         # Economic indicators
                         gdp_per_capita = NY.GDP.PCAP.PP.KD,
                         gdp_growth = NY.GDP.MKTP.KD.ZG,
                         trade_gdp = NE.TRD.GNFS.ZS,
                         fdi_inflows = BX.KLT.DINV.WD.GD.ZS,
                         inflation = FP.CPI.TOTL.ZG,
                         tax_revenue = GC.TAX.TOTL.GD.ZS,
                         
                         # Financial indicators
                         govt_debt = GC.DOD.TOTL.GD.ZS,
                         current_account = BN.CAB.XOKA.GD.ZS,
                         stock_market_cap = CM.MKT.LCAP.GD.ZS,
                         domestic_credit = FS.AST.PRVT.GD.ZS,
                         
                         # Labor market and inequality
                         unemployment = SL.UEM.TOTL.ZS,
                         female_labor_force = SL.TLF.CACT.FE.ZS,
                         gini = SI.POV.GINI,
                         poverty_rate = SI.POV.NAHC,
                         
                         # Health indicators
                         life_expectancy = SP.DYN.LE00.IN,
                         health_expenditure = SH.XPD.CHEX.GD.ZS,
                         measles_immunization = SH.IMM.MEAS,
                         maternal_mortality = SH.STA.MMRT,
                         
                         # Education indicators
                         education_exp = SE.XPD.TOTL.GD.ZS,
                         tertiary_enrollment = SE.TER.ENRR,
                         literacy_rate = SE.ADT.LITR.ZS,
                         
                         # Infrastructure and technology
                         internet_users = IT.NET.USER.ZS,
                         mobile_subscriptions = IT.CEL.SETS.P2,
                         electric_consumption = EG.USE.ELEC.KH.PC,
                         
                         # Environmental indicators
                         protected_areas = ER.PTD.TOTL.ZS,
                         forest_area = AG.LND.FRST.ZS,
                         
                         # Governance indicators
                         property_rights = IQ.CPA.PROT.XQ,
                         region, income]

# Get most recent available data for each country
wb_data_clean <- wb_data_clean %>% group_by(country, iso3c) %>%
  arrange(desc(year)) %>%
  slice(1) %>%
  ungroup()



# Step 2: Merge Datasets
# ---------------------
# Merge World Bank and IMF data by country code
wb_data_clean <- wb_data_clean[region != "Aggregates", ]
combined_data <- wb_data_clean


# Step 3: Data Preprocessing
# -------------------------
# Select numeric variables for clustering
cluster_data <- combined_data %>%
  select(
    # Economic indicators
    gdp_per_capita, gdp_growth, trade_gdp, fdi_inflows, inflation, tax_revenue,
    
    # Financial indicators
    govt_debt, current_account, stock_market_cap, domestic_credit,
    
    # Labor market and inequality
    unemployment, female_labor_force, gini, poverty_rate,
    
    # Health indicators
    life_expectancy, health_expenditure, measles_immunization, maternal_mortality,
    
    # Education indicators
    education_exp, tertiary_enrollment, literacy_rate,
    
    # Infrastructure and technology
    internet_users, mobile_subscriptions, electric_consumption,
    
    # Environmental indicators
    protected_areas, forest_area,
    
    # Governance indicators
    property_rights
  ) %>%
  # Handle remaining NAs by imputation (median)
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Scale the data
scaled_data <- scale(cluster_data)
row.names(scaled_data) <- combined_data$country

# Step 4: Determine Optimal Number of Clusters
# -------------------------------------------
# Elbow method
wss <- fviz_nbclust(scaled_data, kmeans, method = "wss")
print(wss)

# Silhouette method
silhouette <- fviz_nbclust(scaled_data, kmeans, method = "silhouette")
print(silhouette)

# Based on the above, select optimal k (here we'll use k=4 as an example)
k <- 5

# Step 5: Perform K-means Clustering
# ---------------------------------
km_result <- kmeans(scaled_data, centers = k, nstart = 25)

# Add cluster assignment back to the original data
clustered_data <- combined_data %>%
  mutate(cluster = as.factor(km_result$cluster))

# Step 6: Analyze Clusters
# -----------------------
# Calculate cluster means
cluster_means <- clustered_data %>%
  group_by(cluster) %>%
  summarise(across(c( # Economic indicators
    gdp_per_capita, gdp_growth, trade_gdp, fdi_inflows, inflation, tax_revenue,
    
    # Financial indicators
    govt_debt, current_account, stock_market_cap, domestic_credit,
    
    # Labor market and inequality
    unemployment, female_labor_force, gini, poverty_rate,
    
    # Health indicators
    life_expectancy, health_expenditure, measles_immunization, maternal_mortality,
    
    # Education indicators
    education_exp, tertiary_enrollment, literacy_rate,
    
    # Infrastructure and technology
    internet_users, mobile_subscriptions, electric_consumption,
    
    # Environmental indicators
    protected_areas, forest_area,
    
    # Governance indicators
    property_rights),
    ~mean(., na.rm = TRUE)),
    count = n())

print(cluster_means)

# Step 7: Visualize Clusters
# -------------------------
# PCA for dimensionality reduction
pca_result <- prcomp(scaled_data)
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Create a dataframe for plotting
plot_data <- data.frame(
  country = combined_data$country,
  region = combined_data$region,
  income = combined_data$income,
  cluster = as.factor(km_result$cluster),
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2]
)

# Plot clusters using PCA
ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster, shape = region)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = country), size = 2.5, check_overlap = TRUE, vjust = 1.5) +
  labs(
    title = "Country Clusters Based on World Bank and IMF Indicators",
    subtitle = paste("Principal Components 1 & 2 (", 
                     round(100 * sum(variance_explained[1:2]), 1), 
                     "% of variance explained)", sep = ""),
    x = paste0("PC1 (", round(100 * variance_explained[1], 1), "%)"),
    y = paste0("PC2 (", round(100 * variance_explained[2], 1), "%)")
  ) +
  theme_minimal() +
  theme(legend.position = "right") + theme_labinc2()

# Plot regional distribution within clusters
ggplot(plot_data, aes(x = cluster, fill = region)) +
  geom_bar(position = "stack") +
  labs(
    title = "Regional Distribution Within Clusters",
    x = "Cluster",
    y = "Count"
  ) +
  theme_minimal() +
  coord_flip() + theme_labinc2()

# Plot income distribution within clusters
ggplot(plot_data, aes(x = cluster, fill = income)) +
  geom_bar(position = "stack") +
  labs(
    title = "Income Level Distribution Within Clusters",
    x = "Cluster",
    y = "Count"
  ) +
  theme_minimal() +
  coord_flip()  + theme_labinc2()

# Step 8: Advanced Analysis - Hierarchical Clustering
# -------------------------------------------------
# Perform hierarchical clustering
hc <- hclust(dist(scaled_data), method = "ward.D2")

# Plot dendrogram
fviz_dend(hc, k = k, 
          cex = 0.5, 
          palette = "jco", 
          rect = TRUE, 
          rect_fill = TRUE,
          rect_border = "jco", ggtheme = theme_labinc2(),
          labels_track_height = 0.8,
          main = "Hierarchical Clustering Dendrogram") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Compare k-means and hierarchical clustering
hc_clusters <- cutree(hc, k = k)
comparison <- table(kmeans = km_result$cluster, hclust = hc_clusters)
print(comparison)

# Step 9: Export Results
# ---------------------
# Export the clustered data to CSV
write.csv(clustered_data, "country_clusters.csv", row.names = FALSE)

# Step 10: Create Cluster Profiles
# ------------------------------
# Summarize the characteristics of each cluster
cluster_profiles <- clustered_data %>%
  group_by(cluster) %>%
  summarise(
    # Economic indicators
    avg_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),
    avg_gdp_growth = mean(gdp_growth, na.rm = TRUE),
    avg_trade_gdp = mean(trade_gdp, na.rm = TRUE),
    avg_fdi_inflows = mean(fdi_inflows, na.rm = TRUE),
    avg_inflation = mean(inflation, na.rm = TRUE),
    avg_tax_revenue = mean(tax_revenue, na.rm = TRUE),
    
    # Financial indicators
    avg_govt_debt = mean(govt_debt, na.rm = TRUE),
    avg_current_account = mean(current_account, na.rm = TRUE),
    avg_stock_market_cap = mean(stock_market_cap, na.rm = TRUE),
    avg_domestic_credit = mean(domestic_credit, na.rm = TRUE),
    
    # Labor market and inequality
    avg_unemployment = mean(unemployment, na.rm = TRUE),
    avg_female_labor_force = mean(female_labor_force, na.rm = TRUE),
    avg_gini = mean(gini, na.rm = TRUE),
    avg_poverty_rate = mean(poverty_rate, na.rm = TRUE),
    
    # Health indicators
    avg_life_expectancy = mean(life_expectancy, na.rm = TRUE),
    avg_health_expenditure = mean(health_expenditure, na.rm = TRUE),
    avg_measles_immunization = mean(measles_immunization, na.rm = TRUE),
    avg_maternal_mortality = mean(maternal_mortality, na.rm = TRUE),
    
    # Education indicators
    avg_education_exp = mean(education_exp, na.rm = TRUE),
    avg_tertiary_enrollment = mean(tertiary_enrollment, na.rm = TRUE),
    avg_literacy_rate = mean(literacy_rate, na.rm = TRUE),
    
    # Infrastructure and technology
    avg_internet_users = mean(internet_users, na.rm = TRUE),
    avg_mobile_subscriptions = mean(mobile_subscriptions, na.rm = TRUE),
    avg_electric_consumption = mean(electric_consumption, na.rm = TRUE),
    
    # Environmental indicators
    avg_protected_areas = mean(protected_areas, na.rm = TRUE),
    avg_forest_area = mean(forest_area, na.rm = TRUE),
    
    # Governance indicators
    avg_property_rights = mean(property_rights, na.rm = TRUE),
    
    # IMF data
    
    # Categorical summaries
    top_regions = paste(names(sort(table(region), decreasing = TRUE)[1:2]), collapse = ", "),
    top_income_groups = paste(names(sort(table(income), decreasing = TRUE)[1:2]), collapse = ", ")
  )

print(cluster_profiles)

# Create a radar chart for cluster profiles
# First, prepare data for radar chart
radar_data <- cluster_means %>%
  select(-count) %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  mutate(scaled_value = (value - min(value)) / (max(value) - min(value))) %>%
  ungroup()

# Function to create radar chart (if fmsb package is installed)
if (!require("fmsb")) install.packages("fmsb")
library(fmsb)

# Create radar charts for each cluster
for (i in 1:k) {
  cluster_radar <- radar_data %>%
    filter(cluster == i) %>%
    select(variable, scaled_value) %>%
    pivot_wider(names_from = variable, values_from = scaled_value) %>%
    as.data.frame()
  
  # Add max and min rows required by fmsb
  cluster_radar <- rbind(rep(1,ncol(cluster_radar)), rep(0,ncol(cluster_radar)), cluster_radar)
  
  # Plot radar chart
  pdf(paste0("cluster_", i, "_radar.pdf"))
  radarchart(cluster_radar, 
             title = paste("Profile for Cluster", i), 
             pcol = "darkblue", 
             pfcol = scales::alpha("darkblue", 0.5), 
             plwd = 2)
  dev.off()
}

# Show which countries belong to each cluster
countries_by_cluster <- clustered_data %>%
  select(country, region, income, cluster) %>%
  arrange(cluster, country)

print(countries_by_cluster)
