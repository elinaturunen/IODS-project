
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
write.csv(human, file = "human.csv")

# read data
read.table("human.csv", sep = ";", header=TRUE)



