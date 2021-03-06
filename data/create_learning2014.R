# Elina Turunen 26.1.2017 Exercise 2 Data wrangling. 

lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

str(lrn14)

dim(lrn14)

# Regarding dimensions of the data, there are 60 variables and 183 observations. Variables include age, gender, attitude and points.

# print "Attitude" vector
lrn14$Attitude

# divide each number in the column vector
lrn14$Attitude / 10

# create column 'attitude' by scaling the column "Attitude"
lrn14$attitude <- lrn14$Attitude / 10

# Muodosta ensin stra deep surf
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30", "D06", "D15", "D23", "D31")

stra_questions <- c("ST01", "ST09", "ST17", "ST25", "ST04", "ST12", "ST20", "ST28")

surf_questions <- c("SU02", "SU10", "SU18", "SU26", "SU05", "SU13", "SU21", "SU29", "SU08", "SU16", "SU24", "SU32")

install.packages("dplyr")

library("dplyr")

# Tee columns deep_column etc

deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

surface_columns <- select(lrn14, one_of(surf_questions))
lrn14$surf <- rowMeans(surface_columns)

strategic_columns <- select(lrn14, one_of(stra_questions))
lrn14$stra <- rowMeans(strategic_columns)

# Points - ei skaalata? 

# tarvittavien muuttujien valinta
keep_columns <- c("gender", "Age", "attitude", "deep", "stra", "surf", "Points")

learning2014 <- select(lrn14, one_of(keep_columns))

library(dplyr)

# pisteet jotka yli nollan
learning2014 <- filter(learning2014, Points > 0)

# muuttujien ja havaintojen määrä
dim(learning2014)

?write.csv

# set working directory of R session the iods project folder
setwd("~/Desktop/IODS-project/data")

# save data set
write.table(learning2014, file = "learning2014.txt", append = FALSE, quote = TRUE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE)

# read table
read.table("learning2014.txt")

str(learning2014)

head(learning2014)

dim(learning2014)

install.packages("ggplot2")
library(ggplot2)

library(dplyr)

install.packages("GGally")
library(GGally)

# create a plot matrix
p <- ggpairs(learning2014, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
