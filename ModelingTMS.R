#Modeling newly indexed data
data = read.delim2('indexedTMS.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
data = data[c(-1,-2)]

#switches tms_analyte concentrations from character to numeric values for easier 
# mathematical manipulation
data[,5:65]<- sapply(data[,5:65],as.numeric)

nrow(unique(data['new_index']))
# issue, 5005357b of 505936 are unique index numbers because
# of some that share index/year but nor gender/concentrations

data$index = 0
data$index[1] = 1
for (i in 2:length(data$index)) {
  print(i)
  data$index[i] <- ifelse(data$index[i] == data$index[i-1], data$index[i] + 1, data$index[i])
}


nrow(unique(data['index']))
  
#starting over with new dataset name data
head(data)

#install.packages('tidyverse')
library(tidyverse)
data_nested <- data %>% 
  # may have to change what we group by 
  # because of duplicate ids in a year
  # with differing genders
  group_by(new_index) %>%
  nest()

data_unnested <- data_nested %>%
  unnest()

#not identical because when  by grouping by new index,
# some obsrvations in the same year shared the same lab 
# number but were of a different gender
identical(data, data_unnested)

# 
# # Attempt to run MICE method of filling missing values
# #First, remove categorical value
# new_data2 <- new_data[-c(2)]
# # check missingness from data
# pMiss <- function(x){sum(is.na(x))/length(x)*100}
# apply(new_data2, 2, pMiss)
# #look for missing data pattern
# library(mice)
# md.pattern(new_data2)
# 
# 
# 
# #seed missing values
# library(missForest)
# data.mis <- prodNA(new_data, noNA = 0.1)
# #remove non-numerics
# data.mis <- subset(data.mis, select = -c(1:4))
# #
# library(mice)
# imputed_data <- mice(data.mis, m=5, maxit=50, method = 'pmm', seed = 500)

# Deals with missing values 
# replacing NAs with mean of column values
# *** maybe look into different way of doing this so
# when we go to predict
# for (i in 5:ncol(merge)) {
#   #merge[is.na(merge[,i]), i] <- mean(merge[,i], na.rm = TRUE)
#   merge[,i]= ifelse(is.na(merge[,i]),
#                     ave(merge[,i], FUN = function(x) 
#                       mean(x, na.rm = TRUE)), merge[,i])
# }

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
# install.packages('broom')
library(broom)
td <- tidy(classifier)

# plot of p-values vs. coefficient (colored based on p-values)
# install.packages('ggplot2')
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
