# Loading data
df <- read.csv("../Data/crime1.csv")
install.packages("RColorBrewer")  # Install the package
library(RColorBrewer)  #

# Data management
df$narr86 <- as.factor(df$narr86)  # convert to factor
df$nfarr86 <- as.factor(df$nfarr86)  # convert to factor
df$nparr86 <- as.factor(df$nparr86)  # convert to factor
df$black <- as.logical(df$black)  # convert to logical
df$hispan <- as.logical(df$hispan)  # convert to logical
df$born60 <- as.logical(df$born60)  # convert to logical

# Partition data into training and test sets
set.seed(123)
train_idx <- sample(nrow(df), round(0.8 * nrow(df)))
train <- df[train_idx, ]
test <- df[-train_idx, ]

# Fit logistic regression model
model <- glm(narr86 ~ ., data=train, family=binomial())

# Print model summary
summary(model)

# Create a ggplot to visualize the results
library(ggplot2)

# Create a data frame with the actual values and predicted probabilities
plot_data <- data.frame(actual = train$narr86, predicted = predict(model, type = "response"))

# Create a palette with enough colors to map to the levels of train$narr86
my_palette <- brewer.pal(nlevels(train$narr86), "Set1")

ggplot(plot_data, aes(x = predicted, fill = actual)) +
  geom_histogram(binwidth = 0.025, position = "identity", alpha = 0.5) +
  scale_fill_manual(values = my_palette) +
  labs(x = "Predicted probability", y = "Frequency", fill = "Actual value") +
  theme_minimal()
