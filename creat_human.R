# Elina Turunen  
# Exercise 4 (Data Wrangling) and 5 
# Original data source: http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt


# Exercise 4: Data Wrangling

# read the “Human development” and “Gender inequality” datas
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# see the structure and dimensions of the data. Create summaries of the variables.
str(hd)
dim(hd)
summary(hd)

str(gii)
dim(gii)
summary(gii)

colnames(gii)
colnames(hd)

# rename the variables with (shorter) descriptive names

colnames(hd)[1] <- "hdirank"
colnames(hd)[2] <- "country"
colnames(hd)[3] <- "hdi"
colnames(hd)[4] <- "lifeexpbirth"
colnames(hd)[5] <- "expeduyears"
colnames(hd)[6] <- "meaneduyears"
colnames(hd)[7] <- "gnicapita"
colnames(hd)[8] <- "gnicapitarank_minus_hdirank"

colnames(gii)[1] <- "giirank"
colnames(gii)[2] <- "country"                    
colnames(gii)[3] <- "gii"                              
colnames(gii)[4] <- "maternalmortratio"           
colnames(gii)[5] <- "adolbirth"        
colnames(gii)[6] <- "parliamentF"           
colnames(gii)[7] <- "edu2F"  
colnames(gii)[8] <- "edu2M" 
colnames(gii)[9] <- "labF" 
colnames(gii)[10] <- "labM" 

# print out the new column names of the data
colnames(hd)
colnames(gii)

# define new columns edu2ratio and labratio 
gii <- mutate(gii, edu2ratio = edu2F / edu2M)
gii <- mutate(gii, labratio = labF / labM)

# common columns to use as identifiers
join_by <- c("country")

# join datasets by the selected identifiers 
human <- inner_join(hd, gii, by = join_by)

# see column names
colnames(human)

# glimpse data
glimpse(human)

# save data to data folder
write.csv(human, file = "human1.csv", row.names=FALSE)

# read data
#read.table("human.csv", sep = ";", header=TRUE)


# Exercise 5 

library(dplyr)
library(stringr)

# read data
human <- read.table("human1.csv", sep = ",", header=TRUE)
str(human)

# transform the Gross National Income (GNI) variable to numeric 
# look at the structure of GNI column
str(human$gnicapita)

# remove the commas and print out a numeric version of it 
human$gnicapita <- str_replace(human$gnicapita, pattern=",", replace ="") %>% as.numeric(human$GNI)

# select columns to keep
keep <- c("country", "edu2ratio", "labratio", "expeduyears", "lifeexpbirth", "gnicapita", "maternalmortratio", "adolbirth", "parliamentF")
# select the 'keep' columns
human <- select(human, one_of(keep))

# remove rows with missing values 
# print out a completeness indicator
complete.cases(human)

# print out the data with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

# filter out rows with missing values 
human <- filter(human, complete.cases(human))

# remove observations relating to regions
# look at the last 10 observations of human 
tail(human, n = 10L) 

# define the last indice we want to keep
last <- nrow(human) - 7

# choose everything until the last 7 observations
human <- human[1:last, ]

# define the row names of the data by the country names 
rownames(human) <- human$country

# remove the country name column 
human <- select(human, -country)

str(human)

# save the human data including the row names
write.csv(human, file = "human2.csv", row.names=TRUE)


human2 <- read.table("human2.csv", sep = ",", header=TRUE)

str(human2)
rownames(human2)

# change the name of the first column (because there was a problem :) )
colnames(human2)[1] <- "country"
colnames(human2)

# define the row names of the data by the country names 
rownames(human2) <- human2$country
rownames(human2)
str(human2)

# remove the country name column
human2 <- select(human2, -country)


# Data analysis

# read data
# human2 <- read.csv("human2.csv", sep = ",", header=TRUE)

str(human2)
dim(human2)

library(MASS); library(tidyr); library(dplyr); library(ggplot2); library(GGally)

pairs(human)

ggpairs
p <- ggpairs(human2, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p

# draw a histogram of each variable
gather(human2) %>% ggplot(aes(value)) + geom_histogram() + facet_wrap("key", scales = "free")

summary(human2)

# Part 3

# perform principal component analysis (with the SVD method)
pca_human2 <- prcomp(human2)

# draw a biplot of the principal component representation and the original variables
biplot(pca_human2, choices = 1:2, cex = c(0.5, 1.5), sub = "Principal components explaining variance in non-standardized human2 data")

TAI TÄMÄ

# create and print out a summary of pca_human
s <- summary(pca_human2)
s

# rounded percentages of variance captured by each PC
pca_pr <- round(100*s$importance[2,], digits = 1) 

# print out the percentages of variance
pca_pr

# create object pc_lab to be used as axis labels
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")

# draw a biplot
biplot(pca_human2, cex = c(0.5, 1.5), xlab = pc_lab[1], ylab = pc_lab[2], sub = "Principal components explaining variance in non-standardized human2 data")


# Part 4 

# standardize the variables  
human2_std <- scale(human2) 

# print out summaries of the standardized variables
summary(human2_std) 

# perform principal component analysis (with the SVD method)
pca_human2_std <- prcomp(human2_std)
  
# draw a biplot of the principal component representation and the original variables
biplot(pca_human2_std, choices = 1:2, cex = c(0.5, 1.5), sub = "Principal components explaining variance in standardized human2 data")


# create and print out a summary of pca_human
s_std <- summary(pca_human2_std)
s_std

# rounded percentages of variance captured by each PC
pca_pr_std <- round(100*s_std$importance[2,], digits = 1) 

# print out the percentages of variance
pca_pr_std

# create object pc_lab to be used as axis labels
pc_lab_std <- paste0(names(pca_pr_std), " (", pca_pr_std, "%)")

# draw a biplot
biplot(pca_human2_std, cex = c(0.5, 1.5), xlab = pc_lab_std[1], ylab = pc_lab_std[2], sub = "Principal components explaining variance in non-standardized human2 data")


# Part 5

# Give your personal interpretations of the first two principal component 
# dimensions based on the biplot drawn after PCA on the standardized human data.


# Part 6 

install.packages("FactoMineR")
library(FactoMineR)
data("tea")

library(MASS); library(tidyr); library(dplyr); library(ggplot2); library(GGally)

str(tea)
dim(tea)

# bar plot
gather(tea) %>% ggplot(aes(value)) + geom_bar() + facet_wrap("key", scales = "free")

# column names to keep in the dataset
keep_columns <- c("tearoom", "where", "price", "how", "tea.time", "frequency", "SPC")

# select the 'keep_columns' to create a new dataset
tea_use <- select(tea, one_of(keep_columns))

# multiple correspondence analysis 
mca <- MCA(tea_use, graph = FALSE) 

# summary of the model
summary(mca)

# visualize MCA 
plot(mca, invisible=c("ind"), habillage = "quali") 




