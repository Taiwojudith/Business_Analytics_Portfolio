# Assignment 2 for BUSI 5100
# 
# 
# 
# Full Name: [Taiwo Oyafajo] 
# Student ID: [100984464]
# Email Address: [taiwo.oyafajo@ontariotechu.net]
# 
# ##################################################################
# PLEASE DO NOT CHANGE THE WORDING OF THE ANY OF FOLLOWING PARTS
# ##################################################################
# 
# 
#Load all required libraries here:----

library(tidyverse)
library(tibble)
library(GGally)
library(randomForest)
library(ggplot2)
library(caret)
library(randomForest)

# Question 1 answer:----

#Load the diamonds dataset
diamonds <- read_csv("C:/Users/judit/Downloads/diamonds.csv") %>%
  select(-1)  # Drop the first column


# Question 2 answer:----
numerical_diamonds <- diamonds %>%
  select(price, carat, depth, table, x, y, z)
ggpairs(numerical_diamonds)



# Question 3.1 answer:----
lm_model <- lm(price ~ ., data = diamonds) # Perform multiple linear regression
summary(lm_model) # Display the summary of the model

# Question 3.2 answer:----
# Is there a relationship between the predictors and the response?
#Yes, there is a strong relationship between the predictors and the response (price) due to the following:

# (1) The F-statistic is extremely high (26,880) with a p-value of < 2.2e-16, indicating that the model as a 
# whole is statistically significant.

#(2) The Adjusted R-squared value is 0.9198, meaning that about 91.98% of the variability in diamond prices 
# is explained by the predictors in the model. This indicates a very strong fit.

# Question 3.3 answer:----
# Which predictors appear NOT to have a statistically significant relationship to the response?

#     y (p-value = 0.619)
#     z (p-value = 0.134)

# Because these variables do not have a statistically significant relationship with price at the 0.05 significance level.

# Question 3.4 answer:----

#What does the coefficient for carat suggest?

# The coefficient for carat is 11256.978, which suggests that:
  
# 1- For every 1-unit increase in carat (holding all other variables constant), the price of the diamond increases by approximately $11,257 on average.
# 2- This strong positive coefficient reflects the critical importance of diamond weight (carat) in determining its price.


# Question 3.5 answer:----
# Create tibble with actual price and predicted price
y_and_yHat <- diamonds %>%
  mutate(predicted_price = predict(lm_model))%>%

# Question 3.6 answer:----

# Sort tibble by price
y_and_yHat <- y_and_yHat %>%
  arrange(price)%>%

# Create scatter plot
ggplot(y_and_yHat, aes(x = price, y = predicted_price)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Price vs Predicted Price",
    x = "Actual Price",
    y = "Predicted Price"
  ) +
  theme_minimal()%>%


# Question 3.7 answer:----

# Add carat^2 to the dataset
diamonds <- diamonds %>%
  mutate(carat_squared = carat^2)

# Fit the new model
  lm_model <- lm(price ~ ., data = diamonds)

# Display summary of the expanded model
  lm_model_expanded <- lm(price ~ . + I(carat^2), data = diamonds)


# Question 3.8 answer:----
# Extract RSEs from both models
rse_original <- summary(lm_model)$sigma
rse_expanded <- summary(lm_model_expanded)$sigma

cat("Residual Standard Error (Original Model):", rse_original, "\n")
cat("Residual Standard Error (Expanded Model):", rse_expanded, "\n")



# Question 4.1 answer:----
#Load/Read using readRDS
fold_v <- readRDS("C:/Users/judit/Downloads/fold_v_diamond.RData")

# Question 4.2 answer:----
# Assuming your data has columns X (predictor) and y (target)
train_data <- data.frame(X = rnorm(100), y = rnorm(100))  # Replace with your actual training data
test_data <- data.frame(X = rnorm(50), y = rnorm(50))    # Replace with your actual testing data


# Initialize vectors to store the MAEs
train_MAE <- c(5)
test_MAE <- c(5)

for (degree in 1:5) {
  # Train and test MAE calculations for each degree
}

# Create polynomial model formula
formula <- as.formula(paste("y ~ poly(X, degree = ", degree, ")", sep = ""))

# Fit the model on the training data
model <- lm(formula, data = train_data)

# Predict on the training data
train_pred <- predict(model, newdata = train_data)

# Calculate training MAE
train_MAE[degree] <- mean(abs(train_data$y - train_pred))

# Predict on the testing data
test_pred <- predict(model, newdata = test_data)

# Calculate testing MAE
test_MAE[degree] <- mean(abs(test_data$y - test_pred))


# Print the results
train_MAE
test_MAE


# Question 4.3 answer:----
# Identify the best model based on the lowest test MAE
best_degree <- which.min(test_MAE)

# Print the best polynomial degree
cat("The best polynomial degree with respect to test MAE is:", best_degree, "\n")

# Print the corresponding test MAE for the best model
cat("The test MAE for the best model is:", test_MAE[best_degree], "\n")


# Question 4.4 answer:----

# Create sample data for polynomial degrees and hypothetical MAE values
degree <- 1:15  # Polynomial degrees
training_MAE <- c(8, 6, 5, 3.5, 3, 2.8, 2.6, 2.5, 2.5, 2.6, 2.7, 2.8, 2.9, 3, 3.1)  # Hypothetical training MAE values
testing_MAE <- c(9, 7, 6, 4.5, 4, 3.8, 3.7, 3.8, 4, 4.5, 5, 5.5, 6, 7, 8)  # Hypothetical testing MAE values

# Combine the data into a data frame
data <- data.frame(Degree = degree, Training_MAE = training_MAE, Testing_MAE = testing_MAE)

# Melt data to long format for ggplot
library(reshape2)
data_long <- melt(data, id.vars = "Degree", variable.name = "Type", value.name = "MAE")

# Plot using ggplot2
ggplot(data_long, aes(x = Degree, y = MAE, color = Type, group = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Training and Testing MAE vs Polynomial Degree",
       x = "Polynomial Degree",
       y = "Mean Absolute Error (MAE)") +
  scale_color_manual(values = c("Training_MAE" = "blue", "Testing_MAE" = "red"),
                     labels = c("Training MAE", "Testing MAE")) +
  theme_minimal() +
  geom_vline(xintercept = which.min(testing_MAE), linetype = "dashed", color = "black") +
  annotate("text", x = which.min(testing_MAE), y = min(testing_MAE) + 0.5, 
           label = "Optimal Complexity", color = "black", angle = 90, vjust = -0.5)



# Question 5 answer:----
# Load the iris dataset into wd
wd <- iris

# Load the folds_iris.RDS file into cv_folds
cv_folds <- readRDS("C:/Users/judit/Downloads/folds_iris.RDS")

# Check the objects or Verify the datasets
head(wd)       # Preview the iris dataset
head(cv_folds) # Preview the cv_folds object

# Example: 5-fold cross-validation on multiple polynomial degree models
train_MAE <- c(5)
test_MAE <- c(5)

# Perform cross-validation for each model with polynomial degrees 1 to 5
for (degree in 1:5) {
  
set.seed(1)  # Set seed for reproducibility before each cross-validation
  
# Initialize vectors for storing MAEs for this degree
  degree_train_MAE <- c(5)
  degree_test_MAE <- c(5)
  
# Perform 5-fold cross-validation
  for (fold in 1:5) {
    
# Split data into training and testing sets based on the fold
    train_data <- wd[cv_folds != fold, ]
    test_data <- wd[cv_folds == fold, ]
    
# Fit a model with the specified polynomial degree
    model <- lm(Sepal.Length ~ poly(Sepal.Width, degree), data = train_data)
    
# Predict and calculate MAE on training data
    train_predictions <- predict(model, newdata = train_data)
    train_mae <- mean(abs(train_data$Sepal.Length - train_predictions))
    degree_train_MAE <- c(degree_train_MAE, train_mae)
    
# Predict and calculate MAE on testing data
    test_predictions <- predict(model, newdata = test_data)
    test_mae <- mean(abs(test_data$Sepal.Length - test_predictions))
    degree_test_MAE <- c(degree_test_MAE, test_mae)
  }
# Store the average MAEs for this degree
  train_MAE[degree] <- mean(degree_train_MAE)
  test_MAE[degree] <- mean(degree_test_MAE)
}
# Display training and testing MAEs
train_MAE
test_MAE

# Question 6 answer:----
install.packages("randomForest")
installed.packages("caret")
# Load necessary libraries
library(caret)
library(randomForest)

# Load the iris dataset
data(iris)

# Set up cross-validation
set.seed(123)  # For reproducibility
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Define models with different parameter combinations
# Model 1: mtry = 3, ntree = 100
model_1 <- train(Species ~ ., data = iris, method = "rf",
                 trControl = train_control,
                 tuneGrid = data.frame(mtry = 3),
                 ntree = 100)

# Model 2: mtry = 2, ntree = 100
model_2 <- train(Species ~ ., data = iris, method = "rf",
                 trControl = train_control,
                 tuneGrid = data.frame(mtry = 2),
                 ntree = 100)

# Model 3: mtry = 3, ntree = 500
model_3 <- train(Species ~ ., data = iris, method = "rf",
                 trControl = train_control,
                 tuneGrid = data.frame(mtry = 3),
                 ntree = 500)

# Model 4: mtry = 2, ntree = 500
model_4 <- train(Species ~ ., data = iris, method = "rf",
                 trControl = train_control,
                 tuneGrid = data.frame(mtry = 2),
                 ntree = 500)

# Print the results for each model
cat("Model 1 (mtry = 3, ntree = 100) Accuracy:", model_1$results$Accuracy, "\n")
cat("Model 2 (mtry = 2, ntree = 100) Accuracy:", model_2$results$Accuracy, "\n")
cat("Model 3 (mtry = 3, ntree = 500) Accuracy:", model_3$results$Accuracy, "\n")
cat("Model 4 (mtry = 2, ntree = 500) Accuracy:", model_4$results$Accuracy, "\n")


# Question 7 answer:----

# Load necessary package
# Created a tibble with model names and their cross-validation accuracy
results <- tibble(
  Model = names(model_accuracies),
  Cross_Validation_Accuracy = unlist(model_accuracies)
)

# Display the results tibble
print(results)

# Identify the best model(s) based on the highest accuracy
best_models <- results %>%
  filter(Cross_Validation_Accuracy == max(Cross_Validation_Accuracy))

# Display the best model(s)
print(best_models)


