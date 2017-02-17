library(MASS)
data("Boston")

explore the structure and dimensions of the data
str(Boston)
dim(Boston)

show graphical overview of the data
pairs(Boston)

library(MASS)
library(ggplot2)
library(GGally)
library(dplyr)
install.packages("corrplot")
library(corrplot)
# calculate the correlation matrix and round it
cor_matrix <- cor(Boston) %>% round(2)
# print the matrix
cor_matrix
# visualize the matrix
corrplot(cor_matrix, method="circle",  type = "upper")

summary(Boston) 

# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)

# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)

# save the scaled crim as scaled_crim
scaled_crim <- boston_scaled$crim

# summary of the scaled_crim
summary(scaled_crim)

# create a quantile vector of crim and print it
bins <- quantile(scaled_crim)
bins

# create a categorical variable 'crime'
crime <- cut(scaled_crim, breaks = bins, include.lowest = TRUE, label = c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)

# number of rows in the Boston dataset
n <- nrow(boston_scaled)

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]

# create test set 
test <- boston_scaled[-ind,]

# linear discriminant analysis
lda.fit <- lda(crime ~ ., data = train)

# print lda.fit
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 3)


# save the correct classes from test data 
correct_classes <- test$crime

# remove the crime variable from test data 
test <- dplyr::select(test, -crime)

# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

# reload data
data("Boston")

# center and standardize variables
boston_scaled <- scale(Boston)

# euclidean distance matrix 
dist_eu <- dist(Boston, method = "euclidean")

# look at the summary of the distances
summary(dist_eu)

# k-means clustering
km <-kmeans(dist_eu, centers = 4) 

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)

# deal with randomly assigned cluster centres
set.seed(123)

# determine the max number of clusters 
k_max <- 10 

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(dist_eu, k)$tot.withinss})

# visualize the results
plot(1:k_max, twcss, type='b')

# k-means clustering
km <-kmeans(dist_eu, centers = 2) 

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)


# Bonus

library(MASS)
data("Boston")

# center and standardize variables
boston_scaled <- scale(Boston)

# change the object to data frame 
boston_scaled <- as.data.frame(boston_scaled)

# first calculate the distances between the observations 
dist_eu <- dist(boston_scaled, method = "euclidean")  

# k-means clustering
km <-kmeans(dist_eu, centers = 4) 

# linear discriminant analysis
lda.fit <- lda(km$cluster ~ ., data = boston_scaled) 

# print lda.fit
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(km$cluster) 

#  plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 3)

