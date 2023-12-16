# Thomas Soupionis 
# Final Memorandum Part 1

# Loading both datasets
covid_df <- read.csv("../data/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
head(covid_df)
str(covid_df)

ridership_df <- read.csv("../data/MTA_Daily_Ridership_Data__Beginning_2020_20231114.csv")
head(ridership_df)
str(ridership_df)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Convert dates to Date format
covid_df$date_of_interest <- as.Date(covid_df$date_of_interest, format="%m/%d/%Y")
ridership_df$Date <- as.Date(ridership_df$Date, format="%m/%d/%Y")
str(covid_df); str(ridership_df)

# Aggregating COVID-19 data to get total NYC-wide daily cases
## INCLUDING PROBABLE CASES AS PART OF TOTAL ACTUAL COVID CASES CAN SKEW DATA
covid_df$TOTAL_CASES <- covid_df$CASE_COUNT + covid_df$PROBABLE_CASE_COUNT
str(covid_df)

# Identifying columns that contain 'Ridership' in their names
ridership_columns <- names(ridership_df)[grepl("Ridership", names(ridership_df))]

# Replacing NA values with zeros in ridership columns to avoid 'NA' when adding all Riderships together
ridership_df <- ridership_df %>%
  mutate(across(all_of(ridership_columns), ~ifelse(is.na(.), 0, .)))

# Summing up the ridership for each day
ridership_df$Total_Ridership <- rowSums(ridership_df[ridership_columns])
str(ridership_df)
sum(is.na(ridership_df$Total_Ridership)) # no NA's here now

# Merging the datasets on the date
merged_covid_ridership_df <- merge(covid_df, ridership_df, by.x="date_of_interest", by.y="Date")
str(merged_covid_ridership_df)

# Correlation Analysis
# Exploring the relationship between COVID-19 case trends and subway ridership at a city level.
correlation <- cor(merged_covid_ridership_df$TOTAL_CASES, merged_covid_ridership_df$Total_Ridership)
print(correlation) -> # Result: 0.03 - A very weak positive linear correlation
  
# Plotting the correlation
# Note: This plot uses a logarithmic scale, which helps with wide-ranging data but can compress large values.
ggplot(merged_covid_ridership_df, aes(x=Total_Ridership, y=TOTAL_CASES)) +
  geom_point() +
  scale_y_continuous(trans='log10') + # y-axis on log scale 
  geom_smooth(method=lm, color="blue") +
  labs(title="Correlation between COVID-19 Cases and Total Ridership")
## No clear linear pattern; the confidence band around the line of best fit is wide, suggesting high variability.

# Thomas Soupionis 
# Final Memorandum Part 2

library(tidyr)

# Convert ridership data to long format using ridership_columns
ridership_long <- ridership_df %>%
  pivot_longer(
    cols = all_of(ridership_columns),
    names_to = "Type",
    values_to = "Ridership_Count"
  )
str(ridership_long)

# Merging the long format ridership data with COVID data
merged_long <- merge(covid_df, ridership_long, by.x = "date_of_interest", by.y = "Date")
str(merged_long)

# Define a color mapping for each transportation type
color_mapping <- setNames(c("blue", "green", "red", "purple", "orange"), unique(merged_long$Type))

# Plot with facets for each transportation type, showing ridership trend over time
# and COVID-19 case counts
ggplot(merged_long, aes(x = date_of_interest)) +
  geom_line(aes(y = Ridership_Count, color = Type)) +  # Ridership trend line
  geom_line(aes(y = TOTAL_CASES), color = "black", linetype = "dashed") +  # COVID-19 case trend line
  scale_y_continuous(trans = 'log10') +  # Log scale for y-axis
  scale_color_manual(values = color_mapping) +
  facet_grid(~ Type) +  # Separate plots for each transportation type
  labs(title = "COVID-19 Cases and Ridership Trend by Transportation Type",
       y = "Log10(Count)",
       color = "Transportation Type") +
  theme_minimal()

# Facet grid without looking at COVID-19 cases
ggplot(merged_long, aes(x = date_of_interest, y = Ridership_Count, color = Type)) +
  geom_point() +
  scale_y_continuous(trans = 'log10') +
  scale_color_manual(values = color_mapping) +
  facet_grid(~ Type) +
  labs(title = "Date vs. Ridership by Transportation Type")

# Time series plot with log-transformed total cases and total ridership
ggplot(merged_covid_ridership_df, aes(x = date_of_interest)) +
  geom_line(aes(y = log10(TOTAL_CASES), color = "Total Cases")) +
  geom_line(aes(y = log10(Total_Ridership), color = "Total Ridership")) +
  labs(title = "Time Series of Log-Transformed COVID-19 Cases and Total Ridership",
       y = "Log10(Count)",
       color = "Metric") +
  theme_minimal()

# Interpretation and Conclusions:
# - The analysis of public transportation ridership and daily COVID-19 cases in NYC challenges common 
# assumptions about the spread of the pandemic. Despite the expected impact of dense public transit on 
# case surges, the data and visualizations suggest a more nuanced relationship. The very weak positive 
# correlation coefficient of 0.03, supported by visual trends, indicates that the primary driver of 
# COVID-19 case fluctuations was not public transportation but rather factors such as large gatherings 
# and close-knit group interactions.
# - Visualizations from faceted line and scatter plots illustrate varied responses across transportation 
# modes and caution against oversimplified conclusions. They reveal the complexity of mobility patterns and 
# the influence of external factors like public health policies and social behavior on ridership trends. 
# Additionally, the time series analysis does not present a definitive correlation but rather shows cyclic 
# patterns that may be attributed to routine behaviors or seasonal effects.
# - This analysis indicates that while public transit plays a role in urban mobility, its impact on 
# pandemic dynamics is likely overshadowed by a constellation of social and environmental factors, 
# including non-linear interactions, time lags, and the efficacy of public health interventions. The 
# slight reduction in ridership during peak case surges suggests that other factors, potentially including 
# mask mandates, vaccine rollouts, and community spread dynamics, significantly influenced case trends.
# - As the city progresses towards normalcy with recovering ridership levels and a continuous downtrend in 
# COVID-19 cases since 2023, the analysis underscores the multifaceted nature of pandemic spread mechanisms. 
# Direct correlations with single variables, such as public transportation usage, may not capture the full 
# complexity of transmission pathways. 
# - Addressing the limitations of this analysis requires expanding the dataset to include alternative 
# mobility metrics and detailed records of public health measures, employing sophisticated statistical 
# techniques to explore complex relationships and delayed effects, and conducting a detailed examination 
# of data quality and outliers.
# - The next steps in this investigation include employing multivariate time-series models to account 
# for non-linear relationships and time lags, integrating other forms of travel data to capture a 
# comprehensive view of mobility, and assessing the quantitative impact of public health policies on 
# transit use and COVID-19 case trends. This approach is expected to yield a more accurate depiction of 
# how public transportation interplays with the evolving landscape of the pandemic in NYC.
# - Ultimately, this investigation highlights the importance of considering a broad spectrum of societal 
# behaviors in pandemic modeling and response strategies. It calls for a rigorous approach to interpreting 
# trends and correlations, recognizing the complex interplay of factors that influence the spread of COVID-19 
# within urban environments.




