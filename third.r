# read in the data from Freedman.csv
df <- read.csv("../Data/Freedman.csv", na.strings = "NA")

# remove any rows that contain missing values
df <- na.omit(df)

# I removed Jersey City since it was a significant outlier for density and reduced the helpfullness of the graph
#df <- subset(df, city != "Jersey.City")

# select the variables to cluster on
vars <- c("population", "nonwhite", "density", "crime")
df_vars <- df[, vars]

# normalize the data
df_norm <- scale(df_vars)

# perform k-means clustering with k = 3
set.seed(123)
k <- 3
km <- kmeans(df_norm, k)

# print the cluster centers
print(km$centers)

# print the size of each cluster
print(table(km$cluster))

X11()

# plot the data colored by cluster membership
library(ggplot2)
df_cluster <- data.frame(df_vars, cluster = as.factor(km$cluster))
a <- ggplot(df_cluster, aes(x = density, y = crime, color = cluster)) + geom_point()

#ggsave("./saved/densityVsCrime.png", a, width = 8, height = 6, dpi = 300)

# plot the data colored by cluster membership
library(ggplot2)
p <- ggplot(df_cluster, aes(x = nonwhite, y = crime, color = cluster)) + geom_point()

#ggsave("./saved/nonwhiteVsCrime.png", p, width = 8, height = 6, dpi = 300)

# plot the data colored by cluster membership
library(ggplot2)
q <- ggplot(df_cluster, aes(x = population, y = crime, color = cluster)) + geom_point()

#ggsave("./saved/populationVsCrime.png", q, width = 8, height = 6, dpi = 300)


Sys.sleep(10)