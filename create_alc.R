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


