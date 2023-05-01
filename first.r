# load in data and get libraries
dfStates <- read.csv("../Data/USArrests.csv")
library(ggplot2)

# Create new column which is sum of other crimes
dfStates$crime <- dfStates$Murder + dfStates$Assault + dfStates$Rape

# Remove the individual crime columns
dfStates$murder <- NULL
dfStates$assault <- NULL
dfStates$rape <- NULL

# Partition the data
set.seed(1234)
n <- nrow(dfStates)
size <- (n * 0.6)
training_cases <- sample(n, size)
training <- dfStates[training_cases, ]
test <- dfStates[-training_cases, ]


model <- lm(crime ~ UrbanPop, data = test)
summary(model)
# model ouputs crime = 1.778 * UrbanPop + 83.280

p <- ggplot(test, aes(x = UrbanPop, y = crime)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, data = model) +
  labs(title = "Relationship between UrbanPop and Crime",
       x = "Urban Population",
       y = "Crime")

# Save the plot to a PNG file
ggsave("./saved/popVsCrime.png", p, width = 8, height = 6)