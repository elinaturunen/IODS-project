# Elina Turunen 9.2.2017 Exercise 3 Student Alcohol Consumption Data Set


# read data
math <- read.table("student-mat.csv", sep = ";" , header=TRUE)

# structure of the data
str(math)

# dimensions of the data
dim(math)

# read data
por <- read.table("student-por.csv", sep = ";" , header=TRUE)

# structure of the data
str(por)

# dimensions of the data
dim(por)

# access the dplyr library
library(dplyr)

# choose columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join datasets by identifiers
math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))

# see the new column names
colnames(math_por)

# glimpse at the data
glimpse(math_por)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)

library(ggplot2)

# create new column alc_use of average use in weekdays and weekends
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# create new column high_use, TRUE for those whose alc_use is greater than 2
alc <- mutate(alc, high_use = alc_use > 2)

# glimpse at the new combined data
glimpse(alc)

# Save joined and modified data set to the ‘data’ folder
write.csv(alc, file = "alc.csv")

# read data
read.table("alc.csv", sep = ";", header=TRUE)

# Print out the names of the variables 
colnames(alc)

# The Student Alcohol Consumption Data Sets covers alcohol use within secondary school
# students. The data set includes socio-economic variables like sex, age, 
# school, family size and parents' cohabitation status, education and job. There are 
# school-related variables such as weekly study time, educational support, etc. and 
# grades (for first and second period and the final grade). There are also free time 
# related variables such as going out with friends, relationships, Internet access, 
# etc. And of course alcohol consumption and health status.


# choose 4 variables in the data and for each of them
# present your hypothesis about their relationships with alcohol consumption

# number of school absences (absences)
# extra educational support (schoolsup)
# wants to take higher education (higher) 
# high use of alcohol (high_use)

# I hypothesize that high alcohol consumption is (positively) correlated with 
# number of school absences and extra educational support. 
# I hypothesize that those who do not want to take higher education, do not have
# a greater share of high alcohol use.

library(dplyr); library(ggplot2)
install.packages("tidyr")
library(tidyr)
library(GGally)

# Bar plots 
# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = high_use))
# define the plot as a bar plot and draw it
g1 + geom_bar(aes())

g1 <- ggplot(data = alc, aes(x = absences))
g1 + geom_bar(aes())

g1 <- ggplot(data = alc, aes(x = schoolsup))
g1 + geom_bar(aes())

g1 <- ggplot(data = alc, aes(x = higher))
g1 + geom_bar(aes())

# Amount of people having more than three absences roughly decreases when amount of absences increase. Only a couple of people have over 25 and over 40 absences. Over half of the students have 4 absences or less.
# About 50 people of 382 have school support. 
# Most people want to take higher education. Around 50 people do not want to take it. 
# A little more than 100, roughly 120 students, have high alcohol use.

# Boxplot
# initialize plot
g1 <- ggplot(alc, aes(x = high_use, y = absences))
# define the plot as boxplot
g1 + geom_boxplot() + ylab("absences")
 
# initialize a plot  
g1 <- ggplot(alc, x = schoolsup)
# define the plot as a bar plot and draw it
g1 + geom_bar(aes(x = schoolsup, fill = high_use))

g1 <- ggplot(alc, x = higher)
g1 + geom_bar(aes(x = higher, fill = high_use))

# Those who use a lot of alcohol seem to have more absences from school, 
# about 2-9, when those who do not have high use, have 1-5 absences. 
# Then there are some outliers having more absences that don't fit to the boxplots.

# Those who have school support about 13/50 have high alcohol use, 
# when in those who don't have school support little above 100 have high alcohol use,
# when over 200 out of about 330 do not have, so the amount of high use seems to be 
# a little bit greater among students who don't have school support.

# In those who want to do higher education, less than half have high alcohol use, 
# when in those who do not want higher education, half have high alcohol use, 
# though amount is relatively small, about ten students.

# I hypothesized that high alcohol consumption is (positively) correlated with 
# number of school absences and extra educational support. and it seemed that
# those who use more alcohol have also more absences from school. 
# With the school support the relationship was not as expected, students not having
# extra support having a bit greater amount of high alcohol use.
# Hypothesis that high use is not correlated with wanting to take higher education
# proved to be wrong as those who do not want to take higher education had slightly
# greater share of high alcohol use.

# find the model with glm()
m <- glm(high_use ~ absences + schoolsup + higher, data = alc, family = "binomial")
# print out a summary of the model
summary(m)
# print out the coefficients of the model
coef(m)

# It seems that in the logistic regression model only absences were statistically 
# significant, having standard error 0.02325. It seems that absences from school is 
# positively correlated with high alcohol use. The other varibles were negative so 
# it seems that school support and higher education would decrease high use of alcohol.
# Only absences from school are in line with my hypothesis. 

# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI <- confint(m) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)

# The coefficients as odds ratios tell that those who have absences are 1.094 times 
# more likely to have high use (than those who do not have absences). Those who have 
# school support, they are 0.655 as likely to have high use. And those who want to have
# higher education, they are 0.448 as likely to have high use.

# There is probability of 95 % that the value will be on the confidence interval 
# (absence 1.0474022-1.147447, school support 0.3139491-1.319176, higher education 
# 0.1651913-1.236214). Higher education has widest confidence interval and also biggest 
# standard error.

library(dplyr); library(ggplot2)

# fit the model
m <- glm(high_use ~ absences, data = alc, family = "binomial")

# predict() the probability of high_use
probabilities <- predict(m, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# Absences variable had a statistically significant relation with high use of alcohol
# so it is used here.
# Prediction does not capture right amount of high use, not even the magnitude, so the
# predictive power of the model seems to be not very good. The plot shows 
# insufficiency of the prediction.

# define a loss function 
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)

# The share of inaccurately classified individuals (training error) is 0.2879581 that
# is almost a third of all individuals. It seems to match with the prediction power of 
# earlier predictions. Compared to simple guessing strategy the model gives some 
# information based on tested variables, but as it has so little information it is not
# so strong.

# define a loss function 
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data
loss_func(alc$probability, alc$high_use)

# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]

# With 10-fold cross-validation the model had prediction error (inaccurate 
# classifications) 0.2801047 that is almost a third of all individuals. So my model
# does not have a better test set performance (smaller prediction error) compared to
# the model introduced in DataCamp with about 0.26 error. Better model could perhaps 
# be found by testing different explanatory variables and finding other variables that
# are statistically significant and have together more explanatory power. 

![caption](path)
![caption](absences_Rplot.png) TOIMII
![absences](~/Desktop/IODS-project/absences_Rplot.png)

