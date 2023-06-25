#1
# Load necessary packages
library(dplyr)
library(ggplot2)
library(stringr)

#2
# Create a data frame containing information about the Mexican food truck
mexican_truck <- data.frame(
  name = "TacoServe",
  location = c("98th Street", "West Side", "Downtown"),
  cuisine = "Mexican",
  description = "Delicious and Authentic Mexican Cuisine"
)

#3
# Create a function to generate a unique location_id for each truck location
get_location_id <- function(location) {
  location_id <- paste0(
    str_sub(location, 1, 2), # First two characters of the location string
    sample(1000:9999, size = 1) # Random number between 1000 and 9999
  )
  
  return(location_id)
}

#4
# Apply the get_location_id function to the location columns of the data frame 
mexican_truck$location_id <- sapply(
  mexican_truck$location,
  FUN = get_location_id
)

#5
# Create a function to generate a unique menu_id for each menu item
get_menu_id <- function(name, cuisine) {
  menu_id <- paste0(
    str_sub(name, 1, 2), # First two characters of the name string
    str_sub(cuisine, 1, 2), # First two characters of the cuisine string
    sample(1000:9999, size = 1) # Random number between 1000 and 9999
  )
  
  return(menu_id)
}

#6
# Create a data frame containing information about the Mexican food truck's menu
menu <- data.frame(
  name = c("Tacos", "Enchiladas", "Quesadillas", "Nachos", "Burritos"),
  cuisine = "Mexican",
  price = c(10.50, 11.50, 12.50, 12.50, 13.50)
)

#7
# Apply the get_menu_id function to the menu data frame
menu$menu_id <- sapply(
  menu[, c("name", "cuisine")],
  FUN = get_menu_id
)

#8
# Merge the mexican_truck and menu data frames
mexican_truck <- left_join(mexican_truck, menu, by = "cuisine")

#9
# Create a function to calculate the cost of a given menu item
calculate_cost <- function(menu_item){
  cost <- menu_item$price * 1.2
  return(cost)
}

#10
# Apply the calculate_cost function to the menu_id column
mexican_truck$cost <- sapply(
  mexican_truck$menu_id,
  FUN = calculate_cost
)

#11
# Create a function to generate a list of menu items for a given location
get_menu <- function(location_id){
  location_truck <- filter(mexican_truck, location_id == location_id)
  return(location_truck$name)
}

#12
# Apply the get_menu function to get the menu items for each location
mexican_truck$menu_items <- sapply(
  mexican_truck$location_id,
  FUN = get_menu
)

#13
# Create a function to calculate the total sales of the Mexican food truck
calculate_sales <- function(total_sales, cost){
  total_sales <- cost + total_sales
  return(total_sales)
}

#14
# Apply the calculate_sales function to get the total sales for the truck
mexican_truck$total_sales <- sapply(
  mexican_truck$cost,
  FUN = calculate_sales,
  total_sales = 0
)

#15
# Create a summary report to view the total sales of the truck
summary_report <- summarise(
  mexican_truck,
  total_sales = mean(total_sales, na.rm = TRUE)
)

#16
# Visualise the summary report
ggplot(data = summary_report, aes(x = 1, y = total_sales)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(total_sales, 2)),
    hjust = 1.3,
    vjust = 1
  ) +
  scale_y_continuous(labels = scales::dollar) +
  xlab("") +
  ylab("Total Sales") +
  ggtitle("Total Sales for the 'TacoServe' Food Truck")

#17
# Create a function to calculate the revenue of a given location
calculate_revenue <- function(location_id, cost){
  location_truck <- filter(mexican_truck, location_id == location_id)
  revenue <- cost * length(location_truck$cost)
  return(revenue)
}

#18
# Apply the calculate_revenue function to get the total revenue for each location
mexican_truck$revenue <- sapply(
  mexican_truck$location_id,
  FUN = calculate_revenue,
  cost = mexican_truck$cost
)

#19
# Create a summary report to view the total revenue of the truck
summary_report <- summarise(
  mexican_truck,
  total_revenue = mean(revenue, na.rm = TRUE)
)

#20
# Visualise the summary report 
ggplot(data = summary_report, aes(x = 1, y = total_revenue)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(total_revenue, 2)),
    hjust = 1.3,
    vjust = 1
  ) +
  scale_y_continuous(labels = scales::dollar) +
  xlab("") +
  ylab("Total Revenue") +
  ggtitle("Total Revenue for the 'TacoServe' Food Truck")

#21
# Create a function to calculate the profit of the Mexican food truck
calculate_profit <- function(total_revenue, total_sales) {
  profit <- total_revenue - total_sales
  return(profit)
}

#22
# Apply the calculate_profit function to calculate the total profit
mexican_truck$profit <- sapply(
  mexican_truck[, c("total_sales", "total_revenue")],
  FUN = calculate_profit
)

#23
# Create a summary report to view the total profit of the truck
summary_report <- summarise(
  mexican_truck,
  total_profit = mean(profit, na.rm = TRUE)
)

#24
# Visualise the summary report
ggplot(data = summary_report, aes(x = 1, y = total_profit)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(total_profit, 2)),
    hjust = 1.3,
    vjust = 1
  ) +
  scale_y_continuous(labels = scales::dollar) +
  xlab("") +
  ylab("Total Profit") +
  ggtitle("Total Profit for the 'TacoServe' Food Truck")

#25
# Create a function to compute the average rating of the Mexican food truck
calculate_rating <- function(location_id){
  location_truck <- filter(mexican_truck, location_id == location_id)
  average_rating <- mean(location_truck$rating, na.rm = TRUE)
  return(average_rating)
}

#26
# Apply the calculate_rating function to get the average rating for each location
mexican_truck$rating <- sapply(
  mexican_truck$location_id,
  FUN = calculate_rating
)

#27
# Create a summary report to view the average rating of the truck
summary_report <- summarise(
  mexican_truck,
  average_rating = mean(rating, na.rm = TRUE)
)

#28
# Visualise the summary report
ggplot(data = summary_report, aes(x = 1, y = average_rating)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(average_rating, 2)),
    hjust = 1.3,
    vjust = 1
  ) +
  xlab("") +
  ylab("Average Rating") +
  ggtitle("Average Rating for the 'TacoServe' Food Truck")

#29
# Output the data frame to a csv file
write.csv(mexican_truck, "mexican_truck.csv", row.names = FALSE)

#30
# Create a function to generate a list of reviews for a given location
get_reviews <- function(location_id){
  location_truck <- filter(mexican_truck, location_id == location_id)
  return(location_truck$reviews)
}

#31
# Apply the get_reviews function to get the reviews for each location
mexican_truck$reviews <- sapply(
  mexican_truck$location_id,
  FUN = get_reviews
)

#32
# Create a summary report to view the average reviews of the truck
summary_report <- summarise(
  mexican_truck,
  average_reviews = mean(reviews, na.rm = TRUE)
)

#33
# Visualise the summary report
ggplot(data = summary_report, aes(x = 1, y = average_reviews)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(average_reviews, 2)),
    hjust = 1.3,
    vjust = 1
  ) +
  xlab("") +
  ylab("Average Reviews") +
  ggtitle("Average Reviews for the 'TacoServe' Food Truck")

#34
# Create a function to calculate the total traffic for the Mexican food truck
calculate_traffic <- function(total_traffic, reviews){
  total_traffic <- reviews + total_traffic
  return(total_traffic)
}

#35
# Apply the calculate_traffic function to get the total traffic for the truck
mexican_truck$traffic <- sapply(
  mexican_truck$reviews,
  FUN = calculate_traffic,
  total_traffic = 0
)

#36
# Create a summary report to view the total traffic of the truck
summary_report <- summarise(
  mexican_truck,
  total_traffic = mean(traffic, na.rm = TRUE)
)

#37
# Visualise the summary report
ggplot(data = summary_report, aes(x = 1, y = total_traffic)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(total_traffic, 2)),
    hjust = 1.3,
    vjust = 1
  ) +
  xlab("") +
  ylab("Total Traffic") +
  ggtitle("Total Traffic for the 'TacoServe' Food Truck")

#38
# Create a table that contains all the necessary information
taco_table <- mexican_truck[, c("name", "cuisine", "description",
                                "location", "location_id", "menu_items",
                                "price", "cost", "total_sales", "total_revenue",
                                "profit", "rating", "reviews", "traffic")]

#39
# Output the data frame to a csv file
write.csv(taco_table, "taco_table.csv", row.names = FALSE)

#40
# Print the summary of the data frame
taco_table_summary <- summary(taco_table)
print(taco_table_summary)