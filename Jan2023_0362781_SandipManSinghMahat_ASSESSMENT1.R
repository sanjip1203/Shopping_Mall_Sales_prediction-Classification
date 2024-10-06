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
                                                 
 summary(datasets[[1]])#order id and price data type is character so  data is noisy 


 
 
 
 
 
 
 
 
 
    
 
                                               #Data Exploration
                  #Data cleaning and removing misding value and null value and combining all data in a unit 

 library(dplyr)
# Add a 'Month' column to each dataset before combining them
datasets_cleaned <- lapply(names(datasets), function(month) {
  data <- datasets[[month]]
  data$Month <- month  # Add a 'Month' column with the respective month name
  return(data)
})

# Combine all datasets into one data frame
df <- do.call(rbind, datasets_cleaned)



# Check if 'Quantity.Ordered' and 'Price.Each' columns exist
if ("Quantity.Ordered" %in% colnames(df) & "Price.Each" %in% colnames(df)) {
  
  # Ensure 'Quantity.Ordered' and 'Price.Each' are numeric
  df$Quantity.Ordered <- as.numeric(df$Quantity.Ordered)
  df$Price.Each <- as.numeric(df$Price.Each)
  
  # Create the Total.Sales column
  df$Total.Sales <- df$Quantity.Ordered * df$Price.Each
} else {
  stop("Columns 'Quantity.Ordered' or 'Price.Each' not found in the dataset")
}
# Remove rows with missing or NA values in any column
df <- df %>%
  filter(complete.cases(.))  # Removes rows with NA

# Optionally, drop empty strings as well
df<- df %>%
  filter_all(all_vars(. != ""))
# Inspect the cleaned data
head(df)

            









                                                                       #Basic summary 

# Basic summary of the dataset
summary(df)

# Number of records in the combined dataset
num_records <- nrow(df)

# Range of key values in 'Total.Sales', 'Quantity.Ordered', and 'Price.Each'
range_values <- data.frame(
  Total_Sales_Range = range(df$Total.Sales, na.rm = TRUE),
  Quantity_Ordered_Range = range(df$Quantity.Ordered, na.rm = TRUE),
  Price_Each_Range = range(df$Price.Each, na.rm = TRUE)
)

# Basic statistics: Total sales and average quantity ordered
total_sales <- sum(df$Total.Sales, na.rm = TRUE)
avg_quantity <- mean(df$Quantity.Ordered, na.rm = TRUE)

# Display results
cat("Number of records:", num_records, "\n")
cat("Total Sales:", total_sales, "\n")
cat("Average Quantity Ordered:", avg_quantity, "\n")
print(range_values)

# Calculate sales and average for each month
monthly_summary <- df %>%
  group_by(Month) %>%
  summarise(
    Total_Sales = sum(Total.Sales, na.rm = TRUE),
    Average_Quantity = mean(Quantity.Ordered, na.rm = TRUE)
  )

# Sorting the monthly summary by Total Sales
monthly_summary_sorted <- monthly_summary %>%
  arrange(Total_Sales)

# Display the sorted summary
print(monthly_summary_sorted)



                                                #Date and Time Analysis:

#correcting the date format in a proper manner
df$Order.Date <- as.POSIXct(df$Order.Date, format = "%m/%d/%y %H:%M")
df$Month <- format(df$Order.Date, "%m")
df$Day <- format(df$Order.Date, "%d")
df$Hour <- format(df$Order.Date, "%H")











                                               #Data Preprocessing:

#insuring the appropriate format for analysis of coloumns
summary(df)
#converting day hour and month in numerical variable 
df$Month = as.numeric(df$Month)
df$Day = as.numeric(df$Day)
df$Hour = as.numeric(df$Hour)



# Drop the 'Order.Date' column
df <- df %>%
  select(-Order.Date)

# Check the result to ensure 'Order.Date' is removed
head(df)





                                                  #Feature Engineering
# Feature Engineering: Split 'Purchase.Address' into 'Street', 'City', 'State', and 'Zip.Code'
library(tidyr)

# Ensure Purchase.Address is character type and then extract the components using regular expressions
df <- df %>%
  mutate(
    Purchase.Address = as.character(Purchase.Address)  # Ensure it's a character type
  ) %>%
  # Use regular expression to extract Street, City, State, and Zip.Code directly
  extract(
    Purchase.Address, 
    into = c("Street", "City", "State", "Zip.Code"),
    regex = "^(.+),\\s*(.+),\\s*([A-Z]{2})\\s*(\\d{5})$"
  ) %>%
  # Trim whitespace for all fields just in case
  mutate(
    Street = trimws(Street),
    City = trimws(City),
    State = trimws(State),
    Zip.Code = trimws(Zip.Code)
  )








# Count duplicate Order.ID values
duplicate_count <- df %>%
  group_by(Order.ID) %>%
  summarise(Order_Count = n()) %>%
  filter(Order_Count > 1) %>%
  nrow()

# Display the count of duplicate Order.IDs
cat("Number of duplicate Order.IDs:", duplicate_count, "\n")





#assuming order id is as per the same customer and making new dataframe


# Clean and trim whitespace from relevant columns
df <- df %>%
  mutate(
    Order.ID = trimws(Order.ID),  # Trim whitespace
    Street = trimws(Street),  
    City = trimws(City),  
    State = trimws(State),  
    Zip.Code = trimws(Zip.Code)
  )

# Find repeated Order.IDs only
repeated_orders <- df %>%
  group_by(Order.ID) %>%
  filter(n() > 1)  # Only keep Order.IDs that appear more than once

# Create a new data frame with repeated Order.IDs, their products, and total sales
order_id_df <- repeated_orders %>%
  group_by(Order.ID, Street, City, State, Zip.Code) %>%
  summarise(
    Products = paste(unique(Product), collapse = ", "),  # Concatenate product names into a single string
    Repeat_Count = n(),  # Count how many times each repeated Order.ID appears
    Total_Sales = sum(Total.Sales, na.rm = TRUE),  # Sum of Total Sales for each repeated Order.ID
    .groups = 'drop'  # Avoid warning about grouping
  ) %>%
  # Replace empty strings and NA values with "Not Available"
  mutate(
    Order.ID = ifelse(Order.ID == "", "Not Available", Order.ID),
    Street = ifelse(Street == "", "Not Available", Street),
    City = ifelse(is.na(City), "Not Available", City),
    State = ifelse(State == "", "Not Available", State),
    Zip.Code = ifelse(Zip.Code == "", "Not Available", Zip.Code)
  ) %>%
  # Remove rows where Total_Sales is 0
  filter(Total_Sales > 0)

# Display the new data frame 
print(order_id_df)








# Install and load required libraries

library(arules)

# Clean and trim whitespace from relevant columns
df <- df %>%
  mutate(
    Order.ID = trimws(Order.ID),  # Trim whitespace
    Product = trimws(Product)  # Trim Product names
  )

# Convert data into a transaction format
transactions <- df %>%
  group_by(Order.ID) %>%
  summarise(Products = list(unique(Product))) %>%  # Group products by Order.ID
  pull(Products)  # Extract the product lists as a vector

# Convert the list of transactions into transactions class
transactions <- as(transactions, "transactions")

# Display summary of transactions
summary(transactions)


# Applying Apriori algorithm with lower support
rules <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.4))

# Check if rules are generated
if (length(rules) > 0) {
  # Display top rules sorted by lift
  inspect(sort(rules, by = "lift")[1:10])
} else {
  print("No rules found. Try lowering the support or confidence.")
}







# Create the 'time_frame' column based on the 'Hour' column
df <- df %>%
  mutate(
    time_frame = case_when(
      Hour >= 0 & Hour < 8 ~ "Morning",
      Hour >= 8 & Hour < 16 ~ "Evening",
      Hour >= 16 & Hour < 24 ~ "Night",
      TRUE ~ NA_character_  # In case there are any unexpected values
    )
  )

# Inspect the updated dataframe
head(df)










# Load the necessary libraries
library(leaflet)
library(tidygeocoder)
library(dplyr)


# Ensure that your dataframe 'df' has City and State columns
if (!all(c("City", "State") %in% colnames(df))) {
  stop("Dataframe must contain 'City' and 'State' columns.")
}

# Count the number of customers in each city by state
city_counts <- df %>%
  group_by(State, City) %>%
  summarise(Frequency = n(), .groups = 'drop')  # This will give you the number of customers per city by state

# Create a new column combining City and State for geocoding
city_counts <- city_counts %>%
  mutate(Location = paste(City, State, sep = ", "))  # Create a Location column

# Geocode the cities (using OpenStreetMap)
geocoded_cities <- city_counts %>%
  geocode(Location, method = 'osm', full_results = TRUE)  # Get full results for more details

# Check if geocoding was successful and if 'long' and 'lat' are present
if (!all(c("long", "lat") %in% colnames(geocoded_cities))) {
  stop("Geocoding failed: Missing 'long' or 'lat' columns.")
}

# Combine geocoded data with frequency data
geocoded_cities <- geocoded_cities %>%
  select(long, lat, Location) %>%  # Select necessary columns
  left_join(city_counts, by = c("Location" = "Location"))  

# Ensure the combined data has the necessary columns
if (!"Frequency" %in% colnames(geocoded_cities)) {
  stop("Frequency column not found after joining.")
}

head(geocoded_cities)





                                         #Data visualization


library(ggplot2)

# Summarize total sales by state
state_sales_summary <- df %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE))

# Create a bar plot to visualize total sales for each state
ggplot(state_sales_summary, aes(x = State, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Total Sales by State",
    x = "State",
    y = "Total Sales"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 









# Count the total Order IDs by State
order_count_by_state <- df %>%
  group_by(State) %>%
  summarise(Total_Orders = n_distinct(Order.ID)) %>%  # Count unique Order IDs
  arrange(desc(Total_Orders))  # Sort by Total Orders in descending order

# Display the summarized data (optional)
print(order_count_by_state)
# Create a bar plot for total Order IDs by State
ggplot(order_count_by_state, aes(x = reorder(State, -Total_Orders), y = Total_Orders)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Order Counts by State",
       x = "State",
       y = "Total Order Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 









# Load necessary libraries for visualization
library(tidyr)
library(ggplot2)
library(dplyr)

# Create a list of products for each Order.ID
product_list <- repeated_orders %>%
  group_by(Order.ID) %>%
  summarise(Products = list(unique(Product)), .groups = 'drop') 
# Filter out orders with fewer than 2 products
product_list <- product_list %>%
  filter(lengths(Products) > 1)

# Generate all unique pairs of products (product co-occurrence) within the same order
# Use rowwise() to handle each order separately and avoid issues with differing sizes
product_pairs <- product_list %>%
  rowwise() %>%
  mutate(Product_Pairs = list(as.data.frame(t(combn(Products, 2))))) %>%
  unnest(Product_Pairs) %>%
  rename(Product_1 = V1, Product_2 = V2)

# Count the frequency of each product pair (i.e., how often they were purchased together)
co_occurrence <- product_pairs %>%
  count(Product_1, Product_2, name = "Frequency")

# Create the heatmap using ggplot2 with counts displayed
ggplot(co_occurrence, aes(Product_1, Product_2)) +
  geom_tile(aes(fill = Frequency), color = "white") +
  geom_text(aes(label = Frequency), color = "black", size = 3) +  # Add counts as labels
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Product Co-Occurrence Heatmap",
       x = "Products",
       y = "Products",
       fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))












# Summarize the total sales by time_frame and Month
sales_summary <- df %>%
  group_by(Month, time_frame) %>%
  summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE), .groups = 'drop') 

# Create the grouped bar plot
ggplot(sales_summary, aes(x = Month, y = Total_Sales, fill = time_frame)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Sales by Time Frame and Month",
       x = "Month",
       y = "Total Sales") +
  scale_fill_manual(values = c("Morning" = "pink", "Evening" = "red", "Night" = "yellow")) +
  theme_minimal()





# Summarize total sales by time_frame
time_frame_summary <- df %>%
  group_by(time_frame) %>%
  summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE), .groups = 'drop')

# Calculate the percentage of total sales for each time_frame
time_frame_summary <- time_frame_summary %>%
  mutate(Percentage = Total_Sales / sum(Total_Sales) * 100)

# Create pie chart with percentage labels
ggplot(time_frame_summary, aes(x = "", y = Total_Sales, fill = time_frame)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to pie chart
  labs(title = "Total Sales Distribution by Time Frame") +
  scale_fill_manual(values = c("Morning" = "red", "Evening" = "orange", "Night" = "green")) +
  theme_void() +  
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))  










#Sales Trend Over Time
library(ggplot2)

# Plot total sales by month
ggplot(monthly_summary_sorted, aes(x = Month, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Sales by Month", x = "Month", y = "Total Sales") +
  theme_minimal()







# Summarise sales by product 
all_products <- df %>%
  group_by(Product) %>%
  summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))  # No head() function

# Plot all products
ggplot(all_products, aes(x = reorder(Product, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Sales by Product", x = "Product", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
















#Create a bar plot comparing total sales by city to see which cities have the highest sales
city_sales <- df %>%
  group_by(City) %>%
  summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

ggplot(city_sales, aes(x = reorder(City, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Sales by City", x = "City", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











 
# Sales by Time Frame and State
sales_by_time_state <- df %>%
  group_by(State, time_frame) %>%
  summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE), .groups = 'drop')

ggplot(sales_by_time_state, aes(x = State, y = Total_Sales, fill = time_frame)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales by Time Frame and State", x = "State", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ time_frame)




#Customer Segmentation by Purchase
customer_segmentation <- df %>%
  group_by(Order.ID) %>%
  summarise(Total_Sales = sum(Total.Sales), Average_Order_Size = mean(Quantity.Ordered), .groups = 'drop')

ggplot(customer_segmentation, aes(x = Total_Sales, y = Average_Order_Size)) +
  geom_point(alpha = 0.6) +
  labs(title = "Customer Segmentation by Total Sales and Average Order Size", x = "Total Sales", y = "Average Order Size") +
  theme_minimal()








# Plot the cities on a leaflet map
leaflet(geocoded_cities) %>%
  addTiles() %>%  # Add base map tiles
  addCircles(
    lng = ~long, lat = ~lat, 
    popup = ~paste(City, State, "<br>Customers: ", Frequency),  # Show  sum of order.id  in popup
    radius = 10000,  # Adjust circle size as needed
    weight = 1,
    color = ~ifelse(Frequency == 1, "green", ifelse(Frequency == 2, "orange", "red")),  # Direct color assignment based on frequency
    fillOpacity = 0.5
  ) %>%
  addMarkers(
    lng = ~long, lat = ~lat,
    label = ~as.character(Frequency),  # Show customer count in label
    labelOptions = labelOptions(noHide = TRUE, textOnly = FALSE, direction = "auto")
  )









# Load necessary libraries
library(leaflet)
library(tidygeocoder)
library(dplyr)

# Assuming repeated_orders DataFrame has Order.ID, City, and Total.Sales columns

#: Aggregate sales data and collect Order IDs by City
city_sales_map <- repeated_orders %>%
  group_by(City) %>%
  summarise(
    Total_Sales = sum(Total.Sales, na.rm = TRUE),
    Order_Count = n_distinct(Order.ID),  # Count unique Order IDs
    .groups = 'drop'
  )

#  Geocode the City names to get latitude and longitude
city_sales_map <- city_sales_map %>%
  geocode(City, method = 'osm')

# Check the updated data frame
str(city_sales_map)  # Verify the structure to ensure lat/lon exist

# Create the leaflet map
leaflet(city_sales_map) %>%
  addTiles() %>%
  addMarkers(
    lng = ~long, lat = ~lat,
    icon = makeIcon(
      iconUrl = "https://img.icons8.com/ios-filled/50/000000/star.png",  # Star icon
      iconWidth = 30, iconHeight = 30  # Adjust the size of the icon
    ),
    popup = ~paste("City:", City,
                   "<br>Total Sales: $", format(Total_Sales, big.mark = ","), 
                   "<br>Number of Orders:", Order_Count)
  )
