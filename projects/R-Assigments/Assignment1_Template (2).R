# Assignment 1 for BUSI 5100
# 
# 
# 
# Full Name: [Taiwo Oyafajo] 
# Student ID: [100984464]
# Email Address: [taiwo.oyafajo@ontariotechu.net]
# 
# 
# 
#Load all required libraries here:----
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tibble)
library(lubridate)






# Question 1 answer:----

# Question 1 answer:----
#Load the customers_data.csv file
Customers_data <- read_csv("C:/Users/judit/Downloads/customers_data.csv")


#Load the products_data.csv file
Products_data <- read_csv("C:/Users/judit/Downloads/products_data.csv")


#Load the transactions_data.csv file
transactions_data <- read_csv("C:/Users/judit/Downloads/transactions_data.csv")




# Question 2 answer:----
#customer data has Rows: 2,500 ,Columns: 11
Customers_data %>%
  glimpse()

#transaction data has Rows: 15,033 , Columns: 11
transactions_data %>%
  glimpse()


#products data has Rows: 100 ,Columns: 6
Products_data %>%
  glimpse()



# Question 3 answer:----
transactions_data %>%
  filter(Year == 2023)
transaction_2023 <- filter(transactions_data, Year == 2023)

print(transaction_2023)

head(transaction_2023, 10)




# Question 4 answer:----
customer_summary <- transactions_data %>%
  group_by(Customer_ID) %>%
  summarise(
    total_transactions = n(),           
    total_amount_spent = sum(Total_Amount),  .groups = 'drop' 
  )

head(customer_summary, 10)



# Question 5 answer:----

premium_customers <- Customers_data %>%
  inner_join(customer_summary, by = "Customer_ID") %>%
  filter(total_amount_spent >= 10000) %>%
  mutate(total_transactions = total_amount_spent)

view(premium_customers)




# Question 6 answer:----

merged_transactions_products <- transactions_data %>%
  inner_join(Products_data, by = "Product_ID")
 
category_transaction_count <- merged_transactions_products %>%
  group_by(Category)%>%
  summarise( 
    total_transactions = n())

ggplot(category_transaction_count, aes(x=Category, y=total_transactions))+
  geom_bar(stat = "identity")+
  labs(
    title = "Number of Transactions Per Product Category",
    x = "Product Category",
    y= "Total_Transactions "
  )+theme_classic()



# Question 7 answer:----
transactions_data <- transactions_data %>%
  mutate(Discounted_Amount = ifelse(Total_Amount > 100, Total_Amount * 0.9, Total_Amount))

head(transactions_data)



# Question 8 answer:----
merged_data <- transactions_data %>%
  inner_join(Customers_data, by = c("Customer_ID" = "Customer_ID"))

head(merged_data)

location_summary <-  merged_data %>%
  group_by(Location)%>%
  summarise(
    Number_of_Transactions = n(),
    Total_Amount_Spent = sum(Total_Amount)
  ) %>%
  
  arrange(desc(Number_of_Transactions))



# Question 9 answer:----

transaction_2023<- transaction_2023 %>%
mutate(Date = as.Date(Date,format("%m/%d/%Y"))) 

transaction_2023 <- transaction_2023 %>%
mutate(Month = format(Date, "%Y-%m"))


frequent_customers <- transaction_2023 %>%
  group_by(Customer_ID, Month) %>%
  summarise(purchases_in_month = n(), .groups = 'drop') %>%
  filter(purchases_in_month >= 3) %>%
  distinct(Customer_ID)

head(frequent_customers)

#or

transaction_2023 <- transaction_2023 %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),    # Convert Date column to Date format
         Month = format(Date, "%Y-%m"))                # Extract Year-Month from Date

# Step 2: Group by Customer_ID and Month, count purchases per customer per month
frequent_customers <- transaction_2023 %>%
  group_by(Customer_ID, Month) %>%
  summarise(purchases_in_month = n(), .groups = 'drop')  # Count number of purchases per month

# Step 3: Filter customers who made at least 3 purchases in any month
frequent_customers <- frequent_customers %>%
  filter(purchases_in_month >= 3) %>%
  distinct(Customer_ID)  # Keep only distinct Customer_IDs

# Step 4: Print the result - this should show only distinct Customer_IDs
print(frequent_customers)
view(frequent_customers)

#or
# Step 1: Ensure Date is properly formatted and extract Month in "YYYY-MM" format
transaction_2023 <- transaction_2023 %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),    # Convert Date column to Date format
         Month = format(Date, "%Y-%m"))                # Extract Year-Month from Date

# Step 2: Group by Customer_ID and Month, count purchases per customer per month
transactions_summary <- transaction_2023 %>%
  group_by(Customer_ID, Month) %>%
  summarise(purchases_in_month = n(), .groups = 'drop')  # Count number of purchases per month

# Step 3: Filter customers who made at least 3 purchases in any month
frequent_customers <- transactions_summary %>%
  filter(purchases_in_month >= 3) %>%
  distinct(Customer_ID, Month)  # Keep distinct Customer_IDs and Month combinations

# Step 4: Join back to the original data to get full transaction details
frequent_customers_full_info <- transaction_2023 %>%
  semi_join(frequent_customers, by = c("Customer_ID", "Month"))

# Step 5: Display the first few rows of the full transaction information for frequent customers
print(head(frequent_customers_full_info))

# Question 10 answer:----

print(head(transaction_2023))

transactions_summary <- transaction_2023 %>%
  group_by(Customer_ID, Month) %>%
  summarise(purchases_in_month = n(), .groups = 'drop')

print(head(transactions_summary))

frequent_customers_any_month <- transactions_summary %>%
  filter(purchases_in_month >= 3) %>%
  distinct(Customer_ID)

print(frequent_customers_any_month)

#or 
transactions_summary <- transaction_2023 %>%
  group_by(Customer_ID, Month) %>%
  summarise(purchases_in_month = n(), .groups = 'drop')

# Filter customers who made 3 or more purchases in any month
frequent_customers_any_month <- transactions_summary %>%
  filter(purchases_in_month >= 3)

#Join the filtered customers with the original dataset to get full customer information
frequent_customers_full_info <- transaction_2023 %>%
  semi_join(frequent_customers_any_month, by = c("Customer_ID", "Month"))

print(head(frequent_customers_full_info))


# Question 11 answer:----

Electronics_transactions <- transactions_data %>%
  left_join(Products_data, by ="Product_ID") %>%
  filter(Category == "Electronics")

ggplot(Electronics_transactions, aes(x = Quantity, y = Total_Amount, color = Store_Location)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "Yellow") +  
  labs(
    title = "Total Amount vs. Quantity for Electronics Transactions",
    x = "Quantity",
    y = "Total Amount Spent"
  ) +
  theme_minimal()
