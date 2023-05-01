# load in data and get libraries
dfCrime <- read.csv("../Data/UScrime.csv")
library(caret)
library(ggplot2)
X11()

# Remove columns that are not needed for analysis
dfCrime <- dfCrime[,c("M", "So", "Ed", "Po1", "Po2", "LF", "M.F", "Pop", "NW", "U1", "U2", "GDP", "Ineq", "Prob", "Time")]

# Split the data into training and test sets
set.seed(1234)
n <- nrow(dfCrime)
size <- (n * 0.6)
training_cases <- sample(n, size)
training <- dfCrime[training_cases, ]
test <- dfCrime[-training_cases, ]

# Train the knn model
knn_model <- train(Time ~ ., data = training, method = "knn", trControl = trainControl(method = "cv"))

# Predict on test set
predictions <- predict(knn_model, test[, -c(16)])

# Calculate MAPE
mape <- mean(abs((test$Time - predictions) / test$Time))
print(paste("MAPE:", mape))

# Calculate RMSE
rmse <- sqrt(mean((test$Time - predictions) ^ 2))
print(paste("RMSE:", rmse))

# Calculate R-squared
ss_res <- sum((test$Time - predictions) ^ 2)
ss_tot <- sum((test$Time - mean(test$Time)) ^ 2)
r_squared <- 1 - (ss_res / ss_tot)
print(paste("R-squared:", r_squared))

# Compute variable importance measures
vim <- varImp(knn_model, scale = FALSE)

# Print the VIMs
print(vim)

# Visualize the VIMs
p <- ggplot(data = vim, aes(x = reorder(rownames(vim), Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance for Predicting Time Served in State Prisons",
       x = "Predictor",
       y = "Variable Importance") 

ggsave("./saved/timeInPrisonPredictors.png", p, width = 8, height = 6)

# Create plot of relationship between highest VIM variable and Time in prison
#highest_vim_var <- rownames(vim)[which.max(vim$Overall)]
#p2 <- ggplot(data = training, aes_string(x = highest_vim_var, y = "Time")) +
#  geom_point(alpha = 0.5) +
#  geom_smooth(method = "lm", se = FALSE, color = "red") +
#  labs(title = paste0("Relationship between '", highest_vim_var, "' and Time in Prison"),
#       x = highest_vim_var,
#       y = "Time in Prison")

#Sys.sleep(5)