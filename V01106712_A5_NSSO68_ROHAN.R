# Set the working directory and verify it
setwd('C:\\Users\\HP\\Desktop')
getwd()
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA","glue")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for Himachal pradesh data
df <- data %>%
  filter(state_1 == "HP")

# Display dataset info
cat("Dataset Information:\n")
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))

# Sub-setting the data
HPData <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(HPData)))


# HANDLING MISSING VALUES
# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

HPData$Meals_At_Home <- impute_with_mean(HPData$Meals_At_Home)
HPData$No_of_Meals_per_day <- impute_with_mean(HPData$No_of_Meals_per_day)


# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(HPData)))


# CHECK FOR OUTLIERS
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}


outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  HPData <- remove_outliers(HPData, col)
}

# RENAME DISTRICTS AND SECTORS USING CODES FROM APPENDIX OF NSSA 68TH ROUND DATA
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

# Create a data frame for district codes and names for Himachal Pradesh
  district_codes <- c("HP01", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
  district_names <- c("Bilaspur", "Chamba", "Hamirpur", "Kangra", "Kinnaur", "Kullu", "Lahaul and Spiti", "Mandi", "Shimla", "Sirmaur", "Solan", "Una")
  
  districts_df <- data.frame(DistrictCode = district_codes, DistrictName = district_names)
  
  # Print the data frame
  print(districts_df)
  
  
HPData$District <- as.character(HPData$District)
HPData$Sector <- as.character(HPData$Sector)
HPData$District <- ifelse(HPData$District %in% names(district_mapping_df), district_mapping_df[HPData$District], HPData$District)
HPData$Sector <- ifelse(HPData$Sector %in% names(sector_mapping), sector_mapping[HPData$Sector], HPData$Sector)


# Merge HPData with the district mapping data frame
HPData <- HPData %>%
  left_join(district_mapping_df, by = c("District" = "District_Code"))

# Replace the old District column with the new District_Name column
HPData <- HPData %>%
  select(-District) %>%
  rename(District = District_Name)

# Check the mapping
print(unique(HPData$District))

#SUMMARIZING VARIABLES
HPData$total_consumption <- rowSums(HPData[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)


# Summarize and display top and bottom consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- HPData %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")


# DISPLAYING TOP AND BOTTOM 3 DISTRICTS OF CONSUMPTION
cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))



region_summary$Region <- ifelse(region_summary$Region == 1, "RURAL", "URBAN")

cat("Region Consumption Summary:\n")
print(region_summary)

View(HPData)


#A5)Visualization 
#A)histogram to show the distribution of total consumption across different districts in Himhachal Pradesh 
hist(HPData$total_consumption, breaks = 10, col = 'red', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in HIMACHAL PRADESH State")

# Create a ggplot histogram with enhancements
ggplot(HPData, aes(x = total_consumption)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = 'blue', color = 'black', alpha = 0.7) +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(
    title = "Distribution of Total Consumption Across Districts in HIMACHAL PRADESH",
    x = "Total Consumption",
    y = "Density"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(0, max(HPData$total_consumption, na.rm = TRUE), by = 500)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  geom_vline(aes(xintercept = mean(total_consumption, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean(HPData$total_consumption, na.rm = TRUE), y = 0.0007, label = "Mean", color = "red", size = 4, hjust = -0.1)


hp_consumption <- aggregate(total_consumption ~ District, data = HPData, sum) 
View(hp_consumption)


#barplot To visualize consumption per district with district names in himachal pradesh

# Sorting the Bhr_consumption data frame by total_consumption
hp_consumption <- hp_consumption[order(hp_consumption$total_consumption, decreasing = TRUE), ]

barplot(hp_consumption$total_consumption, 
        names.arg = hp_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7)


# Create a ggplot bar plot 
ggplot(hp_consumption, aes(x = reorder(District, total_consumption), y = total_consumption)) +
  geom_bar(stat = "identity", aes(fill = total_consumption), color = 'black', alpha = 0.8) +
  scale_fill_gradient(low = "skyblue", high = "steelblue") +
  coord_flip() +
  labs(
    title = "Total Consumption per District in madhya pradesh",
    x = "District",
    y = "Total Consumption",
    caption = "Data Source: NSSO68"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  ) +
  geom_text(aes(label = round(total_consumption, 1)), hjust = -0.2, size = 4, color = "black", fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))



#B)Plot Total_consumption and Meals_at_home on the himachal pradesh state map

#install.packages("sf")
library(ggplot2) 
library(sf) 
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 


data_map <- st_read("C:\\Users\\HP\\Downloads") 
View(data_map)


data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 

data_map_data <- merge(hp_consumption,data_map,by = "District") 
View(data_map_data)


ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry), color = "white", lwd = 0.2) + 
  scale_fill_gradient(low = "lightblue", high = "red", name = "Total Consumption") + 
  labs(
    title = "Total Consumption by District in himachal pradesh",
    subtitle = "Data from NSSO68",
    caption = "Source: NSSO68"
  ) +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black", fontface = "bold") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )



# Aggregate the Meals_At_Home data by district
hp_meals <- aggregate(Meals_At_Home ~ District, data = HPData, mean) 

# Merge the aggregated data with the data_map data frame
data_map_meals <- merge(hp_meals, data_map, by = "District") 
View(data_map_meals)

# Plot the map with the Meals_At_Home variable
ggplot(data_map_meals) + 
  geom_sf(aes(fill = Meals_At_Home, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Average Meals at Home by District in Himachal pradesh") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")

