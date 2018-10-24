#Logistic Regression of Affected ~ All Metabolites + sex + birth year + lab_no

dataset1 = read.delim2('INMSP2004to2017TMSData.txt', header = TRUE, sep = "|", dec = ",", stringsAsFactor = FALSE)
dataset1 = dataset1[c(1,2,3)]

# unique metabolites
# creates separate columns based on tms_analyte name + concentration value 
# gives 505,936 entries
# Note: this dataset only has lab_no, tms_amalytes, and respective concentration
library(tidyr)
new_dataset1 <- spread(dataset1, tms_analyte, concentration)

# read in data of lab_no, sex, and birth year based on lab_no
# extracted from test_name dataset
dataset2 = read.delim2('lab_sex_year2.0.txt', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
dataset2 = dataset2[c(2,3,4)]

# removes the duplicate rows from lab_no
new_dataset2 <- unique(dataset2[,])

# merges the two datasets to get gender and birth year into metabolite data
# based on similar lab_no
m <- merge(new_dataset1, new_dataset2, by = 'lab_no', all = FALSE)

# just writing dataset here to save it for future use
# write.csv(m, file = "combined_TMS_data.csv")

# Binary variable column
m$Affected <- 0
#Affected ids for comparison
datasetAffected = read.delim2('affected_ids.txt', header = FALSE)

# loops through rows to find affected, 
# merging binary data based on row index
for (i in datasetAffected$V1) {
  z = which(m[,1] == i)
  for (y in z) {
    m[y,78] = 1
  }
}

# m now has all our desired variables
# rearrange dataframe the get gender, birth year, and affected at front
merge <- m[,c(1,76,77,78,2:75)]

# then go on to cut out the year from the lab_no
#must be a character class to manipulate as string
merge$lab_no <- as.character(merge$lab_no)
# cut off the first 4 digits of lab_no (AKA the birth year that we already have)
merge$lab_no <- substr(merge$lab_no, 5, 10)
#change back to a numeric class
merge$lab_no <- as.numeric(merge$lab_no)

# switches tms_analyte concentrations from character to numeric values for easier 
# mathematical manipulation
merge[,5:78]<- sapply(merge[,5:78],as.numeric)

# Deals with missing values 
# replacing NAs with mean of column values
# *** maybe look into different way of doing this so
# when we go to predict
for (i in 5:ncol(merge)) {
  #merge[is.na(merge[,i]), i] <- mean(merge[,i], na.rm = TRUE)
  merge[,i]= ifelse(is.na(merge[,i]),
                             ave(merge[,i], FUN = function(x) 
                               mean(x, na.rm = TRUE)), merge[,i])
}

# Encoding the affected ids as factor
merge$Affected = factor(merge$Affected, levels = c(0, 1))

# time to take sample of 1000 that includes all Affected ("1") ids
# this approach will be to take a small sample of only the
# affected ids rows and columns (69), then take separate random subset of
# 931 from merge that have no affecteds. Lastly, combine the two small sample 
# datasets to get our working dataset of 1000 observations. 

affected_sample <- merge[merge$Affected == 1, ]

#all non_affected
non_affected <- merge[merge$Affected != 1, ]

# within 2004-2010 birth year range (only age range we have affected ids for)
year_range_data <- non_affected[non_affected$birth_year <= 2010,]

# sample from our preferred range
non_affected_sample <- year_range_data[sample(1:nrow(year_range_data), 931, replace = FALSE),]

# combined both sample datasets
library(dplyr)
working_data <- bind_rows(affected_sample, non_affected_sample)

# Fitting Logistic Regression to the Sample set 
classifier = glm(formula = Affected ~ lab_no:birth_year + .,
                 family = binomial(),
                 data = working_data)

# summary of fitted model
summary = summary(classifier)

# better for working with summary coefficients
# puts summary output into a table
library(broom)
td <- tidy(classifier)

# plot of p-values vs. coefficient (colored based on p-values)
library(ggplot2)
ggplot(td, aes(term, p.value, fill = (p.value < 0.05))) + geom_histogram(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# run a 10-fold cross validation on working_data
#install.packages("caret")
#install.packages("klaR")
library(caret)
#library(klaR)

#took our lab_num, gender, and birth_year in case it was affected predctions
# because of continual errors 
num_dat <- working_data[,c(4:78)]
#working_data <- as.numeric(working_data$gender)

#figure out errors

Train <- createDataPartition(num_dat$Affected, p=0.6, list=FALSE)
training <- num_dat[ Train, ]
testing <- num_dat[ -Train, ]

mod_fit <- train(Affected ~ .,  data=training, method="glm", family="binomial")

predict(mod_fit, newdata=testing)
predict(mod_fit, newdata=testing, type="prob")

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit1 <- train(Affected ~ .,  data=working_data, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit1, newdata=testing)
confusionMatrix(data=pred, testing$Class)

#### Time to go into sets based on gender ##### 
#Note: working_data already has our males + females
# boxplot(Affected ~ gender, data = working_data)
# 
# males <- working_data[working_data$gender == "M",]
# 
# females <- working_data[working_data$gender == "F",]




#create boxplots based on gender
g <- ggplot(td, aes(genderF, p.value)) + geom_boxplot()
