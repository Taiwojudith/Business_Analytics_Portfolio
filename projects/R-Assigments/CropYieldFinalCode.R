install.packages("ggpubr")
install.packages("caret")
install.packages("car")
install.packages("Metrics")
install.packages("randomForest")
library(tidyverse)  
library(readr)
library(ggplot2)
library(ggpubr)
library(caret)
library(car)
library(Metrics)
library(randomForest)

Yield_data <- read.csv("C:/Users/judit/Downloads/customers_data.csv") # Read the file
str(Yield_data)      # Check data structure
summary(Yield_data)  # Summary statistics for initial insight

mean(Yield_data$Age, na.rm = TRUE)

crop_yield <- Yield_data$Crop_Yield_MT_per_HA   # Define a variable for crop yield
sum(is.na(Yield_data))  # Check total missing values


# Linear Regression Model

# Plotting the relationship between Average Temperature and Crop Yield
plot_temperature <- ggplot(Yield_data, aes(x = Average_Temperature_C, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.5) +   # Scatter plot with transparent points
  geom_smooth(method = "lm", color = "blue") +  # Add linear regression line
  labs(title = "Average Temperature vs Crop Yield", x = "Average Temperature (°C)", y = "Crop Yield (MT per HA)")
print(plot_temperature)
# Fitting a linear regression model for average temperature
lm.fit.temperature <- lm(crop_yield ~ Average_Temperature_C, data = Yield_data)
summary(lm.fit.temperature) # Display the model summary including R_squared and coefficients


# Plotting the relationship between total precipitation and Crop Yield
plot_precipitation <- ggplot(Yield_data, aes(x = Total_Precipitation_mm, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Total Precipitation vs Crop Yield", x = "Total Precipitation (mm)", y = "Crop Yield (MT per HA)")
print(plot_precipitation)
# Fitting a linear regression model for total precipitation
lm.fit.precipitation <- lm(crop_yield ~ Total_Precipitation_mm, data = Yield_data)
summary(lm.fit.precipitation)


# Plotting the relationship between CO2 emissions and Crop Yield
plot_co2 <- ggplot(Yield_data, aes(x = CO2_Emissions_MT, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "CO2 Emissions vs Crop Yield", x = "CO2 Emissions (MT)", y = "Crop Yield (MT per HA)")
print(plot_co2)
# Fitting a linear regression model for CO2 emissions
lm.fit.co2 <- lm(crop_yield ~ CO2_Emissions_MT, data = Yield_data)
summary(lm.fit.co2)


# Plotting the relationship between Extreme Weather Events and Crop Yield
plot_weather_events <- ggplot(Yield_data, aes(x = Extreme_Weather_Events, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Extreme Weather Events vs Crop Yield", x = "Extreme Weather Events", y = "Crop Yield (MT per HA)")
print(plot_weather_events)
# Fitting a linear regression model for Extreme Weather Events
lm.fit.weather.events <- lm(crop_yield ~ Extreme_Weather_Events, data = Yield_data)
summary(lm.fit.weather.events)


# Plotting the relationship between Irrigation Access and Crop Yield
plot_irrigation <- ggplot(Yield_data, aes(x = Irrigation_Access_., y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Irrigation Access vs Crop Yield", x = "Irrigation Access (%)", y = "Crop Yield (MT per HA)")
print(plot_irrigation)
# Fitting a linear regression model for Irrigation Access
lm.fit.irrigation <- lm(crop_yield ~ Irrigation_Access_., data = Yield_data)
summary(lm.fit.irrigation)


# Plotting the relationship between Pesticide and Crop Yield
plot_pesticide <- ggplot(Yield_data, aes(x = Pesticide_Use_KG_per_HA, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Pesticide Use vs Crop Yield", x = "Pesticide Use (KG per HA)", y = "Crop Yield (MT per HA)")
print(plot_pesticide)
# Fitting a linear regression model for pesticide
lm.fit.pesticide.use <- lm(crop_yield ~ Pesticide_Use_KG_per_HA, data = Yield_data)
summary(lm.fit.pesticide.use)


# Plotting the relationship between Fertilizer and Crop Yield
plot_fertilizer <- ggplot(Yield_data, aes(x = Fertilizer_Use_KG_per_HA, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Fertilizer Use vs Crop Yield", x = "Fertilizer Use (KG per HA)", y = "Crop Yield (MT per HA)")
print(plot_fertilizer)
# Fitting a linear regression model for Fertilizer
lm.fit.fertilizer.use <- lm(crop_yield ~ Fertilizer_Use_KG_per_HA, data = Yield_data)
summary(lm.fit.fertilizer.use)


# Plotting the relationship between Soil health and Crop Yield
plot_soil_health <- ggplot(Yield_data, aes(x = Soil_Health_Index, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Soil Health Index vs Crop Yield", x = "Soil Health Index", y = "Crop Yield (MT per HA)")
print(plot_soil_health)
# Fitting a linear regression model for Soil health
lm.fit.soil.health <- lm(crop_yield ~ Soil_Health_Index, data = Yield_data)
summary(lm.fit.soil.health)


# Plotting the relationship between Economic Impact and Crop Yield
plot_economic_impact <- ggplot(Yield_data, aes(x = Economic_Impact_Million_USD, y = Crop_Yield_MT_per_HA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Economic Impact vs Crop Yield", x = "Economic Impact (Million USD)", y = "Crop Yield (MT per HA)")
print(plot_economic_impact)
# Fitting a linear regression model for Economic Impact
lm.fit.economic.impact <- lm(crop_yield ~ Economic_Impact_Million_USD, data = Yield_data)
summary(lm.fit.economic.impact)


# Arrange all scatter plots in a grid layout
ggarrange(plot_temperature, plot_precipitation, plot_co2, plot_weather_events,
          plot_irrigation, plot_pesticide, plot_fertilizer, plot_soil_health, 
          plot_economic_impact, ncol = 2, nrow = 5)


# Define a function to extract metrics from Regression models
metrics_factor <- function(factor) {
  summary_factor <- summary(factor)   # Model summary
  list(
    Residual_Standard_Error = summary_factor$sigma,   # Measure of model fit
    Adjusted_R_Squared = summary_factor$adj.r.squared  # R-squared adjusted for number of predictors
  )
}

# Collect performance metrics for all individual factors 
metrics_factor <- list(
  Temperature = metrics_factor(lm.fit.temperature),
  Precipitation = metrics_factor(lm.fit.precipitation),
  CO2_Emissions = metrics_factor(lm.fit.co2),
  Extreme_Weather = metrics_factor(lm.fit.weather.events),
  Irrigation = metrics_factor(lm.fit.irrigation),
  Pesticide_Use = metrics_factor(lm.fit.pesticide.use),
  Fertilizer_Use = metrics_factor(lm.fit.fertilizer.use),
  Soil_Health = metrics_factor(lm.fit.soil.health),
  Economic_Impact = metrics_factor(lm.fit.economic.impact)
)

# Combine the performance metrics for all individual regression models into a single data frame
Performance_each_factor <- do.call(rbind, metrics_factor)
# Convert the metrics_factor list into a structured data frame
Performance_each_factor <- data.frame(
  Factor = names(metrics_factor),  # Use names of the list as the Factor column
  Residual_Standard_Error = sapply(metrics_factor, function(x) x$Residual_Standard_Error),
  Adjusted_R_Squared = sapply(metrics_factor, function(x) x$Adjusted_R_Squared),
  stringsAsFactors = FALSE
)

#  Display model comparison metrics
print(Performance_each_factor)


# Calculate the correlation matrix for the specified variables in the Yield_data dataset
cor_matrix <- cor(Yield_data[, c("Average_Temperature_C", "Total_Precipitation_mm", "CO2_Emissions_MT", 
                                 "Extreme_Weather_Events", "Irrigation_Access_.", "Pesticide_Use_KG_per_HA",
                                 "Fertilizer_Use_KG_per_HA", "Soil_Health_Index", "Economic_Impact_Million_USD")])
print(cor_matrix) # Display the matrix of correlation coefficients for analysis
# +1: Perfect positive correlation
# −1: Perfect negative correlation
# 0: No linear relationship


# Fit a multi linear regression model with all predictors
lm_multi_all <- lm(Crop_Yield_MT_per_HA ~ Average_Temperature_C + Total_Precipitation_mm + CO2_Emissions_MT + 
                     Extreme_Weather_Events + Irrigation_Access_. + Pesticide_Use_KG_per_HA + 
                     Fertilizer_Use_KG_per_HA + Soil_Health_Index + Economic_Impact_Million_USD, 
                   data = Yield_data)
summary(lm_multi_all) # Evaluate the model with all variables

#Non significant predictors with high p-values (p>0.05) are:
#Extreme_Weather_Events (p=0.9726)
#Irrigation_Access_. (p=0.8712)
#Pesticide_Use_KG_per_HA (p=0.8444)
#Fertilizer_Use_KG_per_HA (p=0.9804)
#Soil_Health_Index (p=0.5769)
#These predictors do not explain enough variation in crop yield so can cause overfitting and reduce the interpretability of the model
#So in refined model just Retain only the significant predictors that are:
#Average_Temperature_C
#Total_Precipitation_mm
#CO2_Emissions_MT
#Economic_Impact_Million_USD

# Fit a refined multi linear regression model with only significant predictors
lm_multi_refined <- lm(Crop_Yield_MT_per_HA ~ Average_Temperature_C + Total_Precipitation_mm + CO2_Emissions_MT 
                       + Economic_Impact_Million_USD, 
                       data = Yield_data)
summary(lm_multi_refined) # Display the summary of the refined model

# Diagnostic plots for the refined model
par(mfrow = c(2, 2))
plot(lm_multi_refined)

# Create a tibble for observed and predicted values
Crop_yield_prediction <- tibble(Crop_yield = Yield_data$Crop_Yield_MT_per_HA, Predicted_crop_yield = predict(lm_multi_refined))
head(Crop_yield_prediction)

#The model predicts crop yields using factors like temperature 
#precipitation, CO2 emissions and economic impact, with reasonable 
#accuracy but some variability,requiring further validation





# Polynomial regression model

# Create empty vectors to store Mean Absolute Error (MAE) values for training and testing sets
training_MAE <- c()
testing_MAE <- c()

# Polynomial degrees to test
# Define the range of polynomial degrees (1 to 5) to test in the regression model
degrees <- 1:5


set.seed(123)  # For reproducibility
fold_v <- sample(1:10, nrow(Yield_data), replace = TRUE) #Generate a random fold assignment for cross-validation (10 folds)

# Iterate over each polynomial degree to evaluate the model's performance using cross-validation
for (d in degrees) {
  train_MAE <- c()
  test_MAE <- c()
  
  # Loop over each fold
  for (fold in unique(fold_v)) {
    # Split data into training and testing sets based on fold_v
    train_data <- Yield_data[fold_v != fold, ]
    test_data <- Yield_data[fold_v == fold, ]
    
    # Fit polynomial regression model on training data
    # Define and fit a polynomial regression model of degree `d`
    formula <- as.formula(paste("Crop_Yield_MT_per_HA ~ poly(Average_Temperature_C,", d, ", raw=TRUE)"))
    model <- lm(formula, data = train_data)
    
    # Predict on training and testing data
    train_pred <- predict(model, newdata = train_data)
    test_pred <- predict(model, newdata = test_data)
    
    # Calculate MAE for training and testing predictions
    train_MAE <- c(train_MAE, mean(abs(train_data$Crop_Yield_MT_per_HA - train_pred)))
    test_MAE <- c(test_MAE, mean(abs(test_data$Crop_Yield_MT_per_HA - test_pred)))
  }
  
  # Store average MAE for this polynomial degree across all folds
  training_MAE <- c(training_MAE, mean(train_MAE))
  testing_MAE <- c(testing_MAE, mean(test_MAE))
}

# Combine results into a data frame for visualization
results <- data.frame(
  Degree = degrees,
  Training_MAE = training_MAE,
  Testing_MAE = testing_MAE
)

# Print the MAE results for all degrees
print(results)

# Identify the best polynomial degree based on testing MAE(the polynomial degree that minimizes the testing MAE)
best_degree <- results$Degree[which.min(results$Testing_MAE)]
cat("The best polynomial degree is:", best_degree, "\n")
#The best degree is 5, as it has the lowest testing MAE (0.7436), 
#ensuring accurate predictions while maintaining model efficiency


# Create a plot for Training and Testing MAEs
ggplot(results, aes(x = Degree)) +
  geom_line(aes(y = Training_MAE, color = "Training MAE"), size = 1) +
  geom_line(aes(y = Testing_MAE, color = "Testing MAE"), size = 1) +
  geom_point(aes(y = Training_MAE, color = "Training MAE"), size = 2) +
  geom_point(aes(y = Testing_MAE, color = "Testing MAE"), size = 2) +
  labs(title = "Training and Testing MAE for Polynomial Degrees",
       x = "Polynomial Degree", 
       y = "Mean Absolute Error (MAE)") +
  scale_color_manual(name = "Legend", values = c("Training MAE" = "green", "Testing MAE" = "black")) +
  theme_minimal()

# Prediction for degree 4 and 5
# Fit a polynomial regression model of degree 4 and make predictions
degree_4_formula <- as.formula("Crop_Yield_MT_per_HA ~ poly(Average_Temperature_C, 4, raw=TRUE)")
model_4 <- lm(degree_4_formula, data = Yield_data)

# Predictions for the 4th-degree model
predictions_4 <- predict(model_4, newdata = Yield_data)

# Fit a polynomial regression model of degree 5 and make predictions
degree_5_formula <- as.formula("Crop_Yield_MT_per_HA ~ poly(Average_Temperature_C, 5, raw=TRUE)")
model_5 <- lm(degree_5_formula, data = Yield_data)

# Predictions for the 5th-degree model
predictions_5 <- predict(model_5, newdata = Yield_data)

# Combine original and predicted data into a tibble
results_tibble <- tibble(
  Original_Crop_Yield = Yield_data$Crop_Yield_MT_per_HA,
  Predicted_Yield_Degree_4 = predictions_4,
  Predicted_Yield_Degree_5 = predictions_5
)

# Print the tibble
print(results_tibble)

#The table compares original crop yields with predictions from 4th and 5th-degree
#models, with the 5th-degree model slightly more accurate as it is showing slightly
#closer alignment to the original values





# Random Forest model 

# Define hyperparameter sets
models <- list(
  list(mtry = 3, ntree = 300), 
  list(mtry = 4, ntree = 300), 
  list(mtry = 3, ntree = 500), 
  list(mtry = 4, ntree = 500)  
)

# Initialize vector to store cross-validation results
results <- numeric(length(models))

set.seed(123) # Ensure reproducibility

# Perform k-fold cross validation
k <- 5   # Define number of folds for cross validation
cv_folds <- sample(rep(1:k, length.out = nrow(Yield_data)))  # Assign each row of data to a fold

# Initialize a list to store model parameters and RMSE
model_params <- list()

# Loop through each model
for (model_idx in 1:length(models)) {
  model <- models[[model_idx]] # Get current model's hyperparameters
  
  fold_accuracies <- c() # Vector to store accuracy for each fold
  
  for (fold in 1:k) { # Loop through each fold
    # Split data into training and testing sets based on fold number
    train_idx <- which(cv_folds != fold)
    test_idx <- which(cv_folds == fold)
    
    train_data <- Yield_data[train_idx, ]  # Training data
    test_data <- Yield_data[test_idx, ]  # Testing data
    
    # Train the Random Forest model
    rf_model <- randomForest(Crop_Yield_MT_per_HA ~ Average_Temperature_C + Total_Precipitation_mm +
                               CO2_Emissions_MT + Extreme_Weather_Events + Irrigation_Access_. +
                               Pesticide_Use_KG_per_HA + Fertilizer_Use_KG_per_HA + Soil_Health_Index +
                               Economic_Impact_Million_USD,
                             data = train_data, 
                             mtry = model$mtry, 
                             ntree = model$ntree)
    
    # Make predictions on the test set
    predictions <- predict(rf_model, newdata = test_data)
    
    # Calculate RMSE for the fold
    fold_rmse <- sqrt(mean((test_data$Crop_Yield_MT_per_HA - predictions)^2))
    fold_accuracies <- c(fold_accuracies, fold_rmse) # Add RMSE of each fold
  }
  
  # Store average RMSE and model parameters for the current model
  results[model_idx] <- mean(fold_accuracies)
  model_params[[model_idx]] <- list(mtry = model$mtry, ntree = model$ntree) # Store the model parameters
}

# Assign model names to results
names(results) <- c("Model 1", "Model 2", "Model 3", "Model 4")

# Display RMSE for each model
print(results)

# Create a tibble to store model names, RMSE, and parameters
results_tibble <- tibble(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  Cross_Validation_RMSE = results,
  mtry = sapply(model_params, function(x) x$mtry),
  ntree = sapply(model_params, function(x) x$ntree)
)

# Print the results tibble
print(results_tibble)

# Filter the best model with the minimum RMSE
best_models <- results_tibble %>%
  filter(Cross_Validation_RMSE == min(Cross_Validation_RMSE))

# Print the best model information
cat("The best model(s) are:\n")
print(best_models)

# Extract the best model's parameters (mtry and ntree) from best_models tibble
best_model_mtry <- best_models$mtry
best_model_ntree <- best_models$ntree

# Train the best Random Forest model on the full dataset
best_rf_model <- randomForest(Crop_Yield_MT_per_HA ~ Average_Temperature_C + Total_Precipitation_mm +
                                CO2_Emissions_MT + Extreme_Weather_Events + Irrigation_Access_. +
                                Pesticide_Use_KG_per_HA + Fertilizer_Use_KG_per_HA + Soil_Health_Index +
                                Economic_Impact_Million_USD,
                              data = Yield_data, 
                              mtry = best_model_mtry, 
                              ntree = best_model_ntree)

# Make predictions on the entire dataset using the best model
best_model_predictions <- predict(best_rf_model, newdata = Yield_data)

# Create a tibble with original crop yield and predicted crop yield
predictions_tibble <- tibble(
  Original_Crop_Yield = Yield_data$Crop_Yield_MT_per_HA,
  Predicted_Crop_Yield = best_model_predictions
)

# Print the predictions tibble
print(predictions_tibble)

# After analysing all the prediction of the best versions of all the models we have come
# to conclusion that the Random forest model 3 with (mtry = 3, ntree = 500) is giving us
# the closest prediction to the actual crop yeild from the data set.

overall_predictions <- tibble(
  Original_Crop_Yeild = Yield_data$Crop_Yield_MT_per_HA,
  Linear_Model = predict(lm_multi_refined),
  Poly_Model = predictions_5,
  RandomForest_model = best_model_predictions
)

print(overall_predictions)

# Scatter plot to compare predictions
ggplot(overall_predictions, aes(x = Original_Crop_Yeild)) +
  # Add points for each model prediction
  geom_point(aes(y = Linear_Model, color = "Linear Model Prediction"), size = 2, alpha = 0.7) +
  geom_point(aes(y = Poly_Model, color = "Polynomial Model Prediction"), size = 2, alpha = 0.7) +
  geom_point(aes(y = RandomForest_model, color = "Random Forest Prediction"), size = 2, alpha = 0.7) +
  # Add reference line for perfect prediction
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # Add labels and styling
  labs(
    title = "Comparison of Model Predictions to Original Crop Yield",
    x = "Original Crop Yield (MT/HA)",
    y = "Predicted Crop Yield (MT/HA)",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Linear Model Prediction" = "blue",
    "Polynomial Model Prediction" = "green",
    "Random Forest Prediction" = "red"
  )) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )



