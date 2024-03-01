  
#loading caret package
library(caret)
library(ggcorrplot)
library(FactoMineR)
library(corrr)
library(factoextra)

###SOIL MOISTURE

#loading dataset
soil.data <- read.csv("soilmoisture_dataset.csv")

## Visualizations
hist(soil.data$soil_moisture)
hist(soil.data$soil_temperature)

  
## Data cleaning
#dropping unecessary columns
cols_to_drop <- c("index","datetime","soil_temperature")

soil.data <- soil.data[,!(names(soil.data) %in% cols_to_drop)]

#checking for null values
sum(is.na(soil.data))


#getting names of predictors and response variables
predictors <- colnames(soil.data[-1])
response <- "soil_moisture"


## Normalizing data
# Check the scale and range of each predictor variable
summary(soil.data[, !(names(soil.data) %in% response)])

# Check the scale and range of the response variable
summary(soil.data[, response])


# Scale the predictor variables
soil.data[,predictors] <- scale(soil.data[, predictors])

# Scale the response variable
soil.data$soil_moisture <- scale(soil.data$soil_moisture)


#setting seed for reproducing output
set.seed(123)

#split the data into train set (80%) and test set (20%)
trainIndex <- createDataPartition(soil.data$soil_moisture, p = 0.8, list = FALSE )
trainData <- soil.data[trainIndex, ]
testData <- soil.data[-trainIndex, ]


# Train the multiple linear regression model on the training set
lm_model_1 <- lm(formula = paste(response, "~", paste(predictors, collapse="+")), data = trainData)


# Create a residual plot
plot(lm_model_1, which = 1, col = "lightblue", pch = 16, main = "Residual Plot")


#summary report for model
summary(lm_model_1)

# Predict the response variable on the testing set
predictions <- predict(lm_model, newdata = testData)



# Evaluate the model performance on the testing set
RMSE <- sqrt(mean((testData[, response] - predictions)^2))
R_squared <- cor(testData[, response], predictions)^2

# R squared value = 0.855
R_squared

# RMSE value = 0.386
RMSE

cat("RMSE:", RMSE, "\n")
cat("R-squared:", R_squared, "\n")




## Retraining model after PCA
soil.data.2 <-  read.csv("soilmoisture_dataset.csv")
#dropping unecessary columns
cols_to_drop <- c("index","datetime","soil_temperature")
soil.data.2 <- soil.data.2[,!(names(soil.data.2) %in% cols_to_drop)]

#getting names of predictors and response variables
predictors <- colnames(soil.data.2[-1])
response <- "soil_moisture"

# Scale the predictor variables
soil.data.2[,predictors] <- scale(soil.data.2[, predictors])

# Scale the response variable
soil.data.2$soil_moisture <- scale(soil.data.2$soil_moisture)

# Perform PCA
pca <- prcomp(soil.data.2[,predictors] , center = TRUE, scale. = TRUE)

# scree plot for PCA
fviz_eig(pca, addlabels = TRUE)

fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)


#all components
pca_components <- predict(pca, newdata = soil.data.2[,predictors])

# Combine the principal components with the target variable
pca_data <- data.frame(pca_components, soil.data.2$soil_moisture)

# Rename the "soil.data.2.soil_moisture" column to "soil.moisture" using a string
new_name <-  "soil_moisture"
col_to_rename <-  "soil.data.2.soil_moisture"
colnames(pca_data)[colnames(pca_data) == col_to_rename] <- eval(parse(text = substitute(new_name)))


#Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(pca_data$soil_moisture, p = 0.8, list = FALSE)
trainData <- pca_data[trainIndex, ]
testData <- pca_data[-trainIndex, ]


# Graph of RMSE with changing n_components for PCA

# Initialize vectors to store results
num_components <- seq_len(ncol(trainData) - 1)
rmse <- numeric(length(num_components))

# Iterate over different number of components
for (n in num_components[-1]) {
  
  n_components <- n
  pca_components <- predict(pca, newdata = soil.data.2[,predictors])[, 1:n_components]
  
  # Combine the principal components with the target variable
  pca_data <- data.frame(pca_components, soil.data.2$soil_moisture)
  
  # Rename the "soil.data.2.soil_moisture" column to "soil.moisture" using a string
  new_name <-  "soil_moisture"
  col_to_rename <-  "soil.data.2.soil_moisture"
  colnames(pca_data)[colnames(pca_data) == col_to_rename] <- eval(parse(text = substitute(new_name)))
  
  
  # Split the data into training and test sets
  set.seed(123)
  trainIndex <- createDataPartition(pca_data$soil_moisture, p = 0.8, list = FALSE)
  trainData <- pca_data[trainIndex, ]
  testData <- pca_data[-trainIndex, ]
  
  # Train a linear regression model using the principal components
  lm_model <- lm(soil_moisture ~ ., data = trainData)
  
  # Make predictions on the test set
  predictions <- predict(lm_model, testData[, -ncol(testData)])
  
  # Evaluate the model performance
  RMSE <- sqrt(mean((predictions - testData$soil_moisture)^2))
  R_squared <- cor(predictions, testData$soil_moisture)^2
  
  # Calculate RMSE
  rmse[n] <- RMSE
}

# Find the index of the lowest RMSE value
lowest_index <- which.min(rmse[-1])

# Create a line plot
plot(num_components, rmse, type = "l", xlab = "Number of Components", ylab = "RMSE",
     main = "Number of Components vs RMSE", col = "steelblue", lwd = 2)

# Add horizontal line at 0.386
abline(h = 0.386, lty = "dashed", col = "red", lwd = 2)

# Add marker at the lowest point
points(num_components[lowest_index], rmse[lowest_index], col = "red", pch = 16, cex = 1.5)

# Draw vertical line from x-axis to the lowest point
segments(num_components[lowest_index], 0, num_components[lowest_index], rmse[lowest_index], lty = "dashed", col = "blue", lwd = 2)

# Calculate the center of the dotted line
center_x <- (num_components[1] + num_components[length(num_components)]) / 2
center_y <- 0.386

# Add text box for RMSE value
text(x = center_x, y = center_y + 0.01, labels = "RMSE = 0.386", pos = 3, cex = 1, font = 2)

# Add gridlines
grid(lty = "dotted", col = "gray")

# Customize axes
axis(side = 1, col = "gray", lwd = 2)
axis(side = 2, col = "gray", lwd = 2)

# Add labels to axes
text(x = max(num_components), y = -0.02, labels = "Number of Components", pos = 1, col = "gray", font = 2)
text(x = -1, y = 0.386, labels = "RMSE", pos = 2, col = "gray", font = 2)

# Add legend
legend("topright", c("Base RMSE", "Lowest RMSE"), col = c("red", "blue"), pch = c(NA, 16),
       lty = c("dashed", "solid"), lwd = 2, bty = "n", cex = 0.8)

# Add arrow to the red point representing the lowest RMSE
lowest_rmse <- rmse[lowest_index]
text_x <- num_components[lowest_index]
text_y <- lowest_rmse
arrows(text_x, text_y, text_x, text_y, code = 2, angle = 30, length = 0.1)

# Add text with lowest RMSE value at the tail end of the arrow
text(text_x, text_y, labels = sprintf("RMSE = %.3f", lowest_rmse), pos = 3, cex = 0.8, col = "red")


#Getting R squared value for n=40 components


# Get the first n principal components
n_components <- 40
pca_components <- predict(pca, newdata = soil.data.2[,predictors])[, 1:n_components]

# Combine the principal components with the target variable
pca_data <- data.frame(pca_components, soil.data.2$soil_moisture)

# Rename the "soil.data.2.soil_moisture" column to "soil.moisture" using a string
new_name <-  "soil_moisture"
col_to_rename <-  "soil.data.2.soil_moisture"
colnames(pca_data)[colnames(pca_data) == col_to_rename] <- eval(parse(text = substitute(new_name)))


#Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(pca_data$soil_moisture, p = 0.8, list = FALSE)
trainData <- pca_data[trainIndex, ]
testData <- pca_data[-trainIndex, ]

# # Train a linear regression model using the principal components
lm_model_2 <- lm(soil_moisture ~ ., data = trainData)

summary(lm_model_2)
# 

plot(lm_model, which = 1, col = "lightblue", pch = 16, main = "Residual Plot")


# # Make predictions on the test set
predictions <- predict(lm_model, testData[, -ncol(testData)])
# 
# # Evaluate the model performance

R_squared <- cor(predictions, testData$soil_moisture)^2
cat("R-squared:", R_squared, "\n")


#ANOVA to compare both models

# Compare the two models using ANOVA
anova_result <- anova(lm_model_1, lm_model_2)

anova_result

##computing correlation and training model with correlated features
soil.data.3 <- read.csv("soilmoisture_dataset.csv")
  
cols_to_drop <- c("index","datetime","soil_temperature")
soil.data.3 <- soil.data.3[,!(names(soil.data.3) %in% cols_to_drop)]

#checking for null values
sum(is.na(soil.data))


#getting names of predictors and response variables
predictors <- colnames(soil.data.3[-1])
# response <- "soil_moisture"

# Calculate correlation coefficients between each independent variable and the dependent variable
correlations <- sapply(soil.data.3[,predictors], function(x) cor(x, soil.data.3$soil_moisture))

correlations <- sort(correlations, decreasing = TRUE)

# Print the correlation coefficients
print(correlations)


###SOIL TEMPERATURE

#loading dataset
soil.data <- read.csv("soilmoisture_dataset.csv")

## Visualizations
hist(soil.data$soil_moisture)
hist(soil.data$soil_temperature)


## Data cleaning
#dropping unecessary columns
cols_to_drop <- c("index","datetime","soil_moisture")
soil.data <- soil.data[,!(names(soil.data) %in% cols_to_drop)]

#checking for null values
sum(is.na(soil.data))


#getting names of predictors and response variables
predictors <- colnames(soil.data[-1])
response <- "soil_temperature"


## Normalizing data
# Check the scale and range of each predictor variable
summary(soil.data[, !(names(soil.data) %in% response)])

# Check the scale and range of the response variable
summary(soil.data[, response])


# Scale the predictor variables
soil.data[,predictors] <- scale(soil.data[, predictors])

# Scale the response variable
soil.data$soil_temperature <- scale(soil.data$soil_temperature)


#setting seed for reproducing output
set.seed(123)

#split the data into train set (80%) and test set (20%)
trainIndex <- createDataPartition(soil.data$soil_temperature, p = 0.8, list = FALSE )
trainData <- soil.data[trainIndex, ]
testData <- soil.data[-trainIndex, ]

# Train the multiple linear regression model on the training set
lm_model_1 <- lm(formula = paste(response, "~", paste(predictors, collapse="+")), data = trainData)

summary(lm_model_1)

plot(lm_model, which = 1, col = "lightblue", pch = 16, main = "Residual Plot")


# Predict the response variable on the testing set
predictions <- predict(lm_model, newdata = testData)


# Evaluate the model performance on the testing set
RMSE <- sqrt(mean((testData[, response] - predictions)^2))
R_squared <- cor(testData[, response], predictions)^2


cat("RMSE:", RMSE, "\n")
cat("R-squared:", R_squared, "\n")


## Retraining model after PCA
soil.data.2 <-  read.csv("soilmoisture_dataset.csv")

## Data cleaning
#dropping unecessary columns
cols_to_drop <- c("index","datetime","soil_moisture")
soil.data.2 <- soil.data.2[,!(names(soil.data.2) %in% cols_to_drop)]

#checking for null values
sum(is.na(soil.data.2))


#getting names of predictors and response variables
predictors <- colnames(soil.data.2[-1])
response <- "soil_temperature"


## Normalizing data

# Scale the predictor variables
soil.data.2[,predictors] <- scale(soil.data.2[, predictors])

# Scale the response variable
soil.data.2$soil_temperature <- scale(soil.data.2$soil_temperature)


# Perform PCA
pca <- prcomp(soil.data.2[,predictors] , center = TRUE, scale. = TRUE)

# View the summary of PCA results
fviz_eig(pca, addlabels = TRUE)  

# Graph of the variables
fviz_pca_var(pca, col.var = "black")



# Graph of RMSE with changing n_components for PCA

# Initialize vectors to store results
num_components <- seq_len(ncol(trainData) - 1)
rmse <- numeric(length(num_components))

# Iterate over different number of components
for (n in num_components[-1]) {
  
  n_components <- n
  pca_components <- predict(pca, newdata = soil.data.2[,predictors])[, 1:n_components]
  
  # Combine the principal components with the target variable
  pca_data <- data.frame(pca_components, soil.data.2$soil_temperature)
  
  # Rename the "soil.data.2.soil_moisture" column to "soil.moisture" using a string
  new_name <-  "soil_temperature"
  col_to_rename <-  "soil.data.2.soil_temperature"
  colnames(pca_data)[colnames(pca_data) == col_to_rename] <- eval(parse(text = substitute(new_name)))
  
  
  #Split the data into training and test sets
  set.seed(123)
  trainIndex <- createDataPartition(pca_data$soil_temperature, p = 0.8, list = FALSE)
  trainData <- pca_data[trainIndex, ]
  testData <- pca_data[-trainIndex, ]
  
  # Train a linear regression model using the principal components
  lm_model <- lm(soil_temperature ~ ., data = trainData)
  
  # Make predictions on the test set
  predictions <- predict(lm_model, testData[, -ncol(testData)])
  
  # Evaluate the model performance
  RMSE <- sqrt(mean((predictions - testData$soil_temperature)^2))
  R_squared <- cor(predictions, testData$soil_temperature)^2
  
  # Calculate RMSE
  rmse[n] <- RMSE
}


# Find the index of the lowest RMSE value
lowest_index <- which.min(rmse[-1])

# Create a line plot
plot(num_components, rmse, type = "l", xlab = "Number of Components", ylab = "RMSE",
     main = "Number of Components vs RMSE", col = "steelblue", lwd = 2)

# Add horizontal line at 0.386
abline(h = 0.473, lty = "dashed", col = "red", lwd = 2)

# Add marker at the lowest point
points(num_components[lowest_index], rmse[lowest_index], col = "red", pch = 16, cex = 1.5)

# Draw vertical line from x-axis to the lowest point
segments(num_components[lowest_index], 0, num_components[lowest_index], rmse[lowest_index], lty = "dashed", col = "blue", lwd = 2)

# Calculate the center of the dotted line
center_x <- (num_components[1] + num_components[length(num_components)]) / 2
center_y <- 0.473

# Add text box for RMSE value
text(x = center_x, y = center_y + 0.01, labels = "RMSE = 0.473", pos = 3, cex = 1, font = 2)

# Add gridlines
grid(lty = "dotted", col = "gray")

# Customize axes
axis(side = 1, col = "gray", lwd = 2)
axis(side = 2, col = "gray", lwd = 2)

# Add labels to axes
text(x = max(num_components), y = -0.02, labels = "Number of Components", pos = 1, col = "gray", font = 2)
text(x = -1, y = 0.473, labels = "RMSE", pos = 2, col = "gray", font = 2)

# Add legend
legend("topright", c("Base RMSE", "Lowest RMSE"), col = c("red", "blue"), pch = c(NA, 16),
       lty = c("dashed", "solid"), lwd = 2, bty = "n", cex = 0.8)


# Add arrow to the red point representing the lowest RMSE
lowest_rmse <- rmse[lowest_index]
text_x <- num_components[lowest_index]
text_y <- lowest_rmse
arrows(text_x, text_y, text_x, text_y, code = 2, angle = 30, length = 0.1)

# Add text with lowest RMSE value at the tail end of the arrow
text(text_x, text_y, labels = sprintf("RMSE = %.3f", lowest_rmse), pos = 3, cex = 0.8, col = "red")


# Get the first n principal components
n_components <- 38
pca_components <- predict(pca, newdata = soil.data.2[,predictors])[, 1:n_components]

# Combine the principal components with the target variable
pca_data <- data.frame(pca_components, soil.data.2$soil_temperature)

# Rename the "soil.data.2.soil_moisture" column to "soil.moisture" using a string
new_name <-  "soil_temperature"
col_to_rename <-  "soil.data.2.soil_temperature"
colnames(pca_data)[colnames(pca_data) == col_to_rename] <- eval(parse(text = substitute(new_name)))


#Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(pca_data$soil_temperature, p = 0.8, list = FALSE)
trainData <- pca_data[trainIndex, ]
testData <- pca_data[-trainIndex, ]

# Train a linear regression model using the principal components
lm_model_2 <- lm(soil_temperature ~ ., data = trainData)

plot(lm_model, which = 1, col = "lightblue", pch = 16, main = "Residual Plot")

summary(lm_model_2)

#ANOVA to compare both models

# Compare the two models using ANOVA
anova_result <- anova(lm_model_1, lm_model_2)

anova_result


# Make predictions on the test set
predictions <- predict(lm_model, testData[, -ncol(testData)])

# Evaluate the model performance
R_squared <- cor(predictions, testData$soil_temperature)^2
cat("R-squared:", R_squared, "\n")




##computing correlation and training model with correlated features

soil.data.3 <- read.csv("soilmoisture_dataset.csv")

cols_to_drop <- c("index","datetime","soil_moisture")
soil.data.3 <- soil.data.3[,!(names(soil.data.3) %in% cols_to_drop)]

#checking for null values
sum(is.na(soil.data.3))


#getting names of predictors and response variables
predictors <- colnames(soil.data.3[-1])
response <- "soil_temperature"


# Calculate correlation coefficients between each independent variable and the dependent variable
correlations <- sapply(soil.data.3[,predictors], function(x) cor(x, soil.data.3$soil_temperature))

correlations <- sort(correlations, decreasing = TRUE)

# Print the correlation coefficients
print(correlations)
