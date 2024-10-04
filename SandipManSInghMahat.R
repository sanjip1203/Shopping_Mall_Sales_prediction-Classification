# Set working directory
setwd("/Users/sandipmahata/Desktop/data mining/Sales Dataset")

# Importing all datasets into a list
datasets <- list(
  January = read.csv("Sales_January_2019.csv"),
  February = read.csv("Sales_February_2019.csv"),
  March = read.csv("Sales_March_2019.csv"),
  April = read.csv("Sales_April_2019.csv"),
  May = read.csv("Sales_May_2019.csv"),
  June = read.csv("Sales_June_2019.csv"),
  July = read.csv("Sales_July_2019.csv"),
  August = read.csv("Sales_August_2019.csv"),
  September = read.csv("Sales_September_2019.csv"),
  October = read.csv("Sales_October_2019.csv"),
  November = read.csv("Sales_November_2019.csv"),
  December = read.csv("Sales_December_2019.csv")
)

# Inspect the first dataset
head(datasets[[1]])

# Print summary of a sample dataset
summary(datasets[[1]])

# Loading required libraries for data manipulation and visualization
library(dplyr)
library(ggplot2)
library(tidyr)  # Added for `separate` function

# Function to remove rows with non-numeric Order IDs and ensure correct numeric types
clean_dataset <- function(data) {
  data %>%
    filter(grepl("^[0-9]+$", Order.ID)) %>%  # Keep rows where Order.ID is numeric
    mutate(
      Order.ID = as.numeric(Order.ID),
      Quantity.Ordered = as.numeric(Quantity.Ordered),
      Price.Each = as.numeric(Price.Each)
    )
}

# Apply the cleaning function to all datasets
datasets_cleaned <- lapply(datasets, clean_dataset)

# Print summary of the cleaned January dataset
summary(datasets_cleaned[[1]])

# Combine all cleaned datasets into one dataset for the entire year
datasets_combined <- do.call(rbind, datasets_cleaned)



# List of month names corresponding to each dataset
month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# Add a Month column to each dataset before combining
for (i in seq_along(datasets_cleaned)) {
  datasets_cleaned[[i]]$Month <- month_names[i]
}

# Now combine all datasets with the Month column
datasets_combined <- do.call(rbind, datasets_cleaned)

# Create a summary data frame for total sales and average quantity ordered across all months
summary_df <- data.frame(
  Month = month_names,
 
  Average_Quantity = sapply(datasets_cleaned, function(data) mean(data$Quantity.Ordered, na.rm = TRUE))
)

# View the summary data frame
print(summary_df)

# Data Preprocessing
# Convert "Order Date" to Date format and extract components (Month, Day, Hour) for all datasets
for (i in 1:length(datasets_cleaned)) {
  datasets[[i]]$Order.Date <- as.POSIXct(datasets[[i]]$Order.Date, format = "%m/%d/%y %H:%M")
  datasets[[i]]$Month <- format(datasets[[i]]$Order.Date, "%m")
  datasets[[i]]$Day <- format(datasets[[i]]$Order.Date, "%d")
  datasets[[i]]$Hour <- format(datasets[[i]]$Order.Date, "%H")
  

  
}



# Inspect the changes in the first dataset
head(datasets[[1]])

library(stringr)

# Updated function to process sales data
process_sales_data <- function(data) {
  # Create Total Sales column
  data$Total.Sales <- data$Quantity.Ordered * data$Price.Each
  
  # Check if 'Purchase.Address' exists and is properly formatted
  data <- data %>%
    mutate(
      Purchase.Address = as.character(Purchase.Address)  # Ensure it's a character type
    ) %>%
    separate(Purchase.Address, into = c("Street", "City", "State.Zip"), sep = ",", extra = "merge", fill = "right") %>%
    separate(State.Zip, into = c("State", "Zip.Code"), sep = " ", extra = "merge", fill = "right")
  
  return(data)
}

# Re-apply the function to all datasets
datasets_processed <- lapply(datasets_cleaned, process_sales_data)

# Verify the changes in the first dataset
head(datasets_processed[[1]])



# Data Visualization 


# Add a Month column to each dataset and calculate Total Sales for each record
for (i in seq_along(datasets_cleaned)) {
  datasets_cleaned[[i]]$Total.Sales <- datasets_cleaned[[i]]$Price.Each * datasets_cleaned[[i]]$Quantity.Ordered
  datasets_cleaned[[i]]$Month <- month_names[i]  # Assign corresponding month name
}

# Combine all datasets with the Total Sales and Month columns
datasets_combined <- do.call(rbind, datasets_cleaned)

# Ensure the Month column is treated as a factor for correct plotting
datasets_combined$Month <- factor(datasets_combined$Month, levels = month_names)

# Summarize total sales by month
monthly_sales <- datasets_combined %>%
  group_by(Month) %>%
  summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE))

#Create a bar plot for total sales across all months
ggplot(monthly_sales, aes(x = Month, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "red", color = "darkblue") +
  labs(
    title = "Total Sales for Each Month (2019)",
    x = "Month",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 





                              #data visualization 



















