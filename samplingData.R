#Sample data/Imputation of missing affected data

data = read.delim2('goodData.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
data = data[c(-1)]

library(dplyr)
#used later for imputing values (has missing data)
affectedData <- select(filter(data, Affected == 1), c(1:65))
#use for getting control groups (will remove missing data below)
nonAffectedData <- select(filter(data, Affected == 0), c(1:65))

library(plyr)
# gets rid of two unneeded categories for gender and allows to remove all nonAffected
# data missing values below 
nonAffectedData$gender <- revalue(nonAffectedData$gender, c("U"= NA))
nonAffectedData$gender[nonAffectedData$gender==""] <- NA

# library(dplyr) 
#remove rows with missing values so we do not have to impute
#add back in the affected data since it includes NAs so we can sample 
# based on all NA ids
# #sort by birth_year then lab_no
newdata <- nonAffectedData[rowSums(is.na(nonAffectedData)) == 0,] %>%
           dplyr::union(., affectedData) %>%
           arrange(., birth_year, new_index)

# changing of variable types
newdata[,c(5:65)]<- sapply(newdata[,5:65],as.numeric)

newdata$gender = as.factor(newdata$gender)

newdata$Affected = as.factor(newdata$Affected)

#Pull a 50 observation control group based on distance from each affected id (+-200)
#and store into a list to use as a total control group

#while loop to make sure we pass KS Test
#Control group 1
ks1 = 0
ks1$p.value = 0
while(ks1$p.value < 0.2) {
  #print("here")
  temp1 = vector()
  for (i in affectedData$new_index) {
    #print(i)
    # y here is the row number of affected ids in the full dataset
    y1 = which(newdata$new_index == i) 
    #print(y1)
    temp1 = append(temp1, newdata[sample((y1-250):(y1+250), 50, replace = FALSE), "new_index"])
  }
  temp1 <- sort(temp1)
  control1 <- as.data.frame(temp1)
  #make sure there are no duplicate control values
  control1 <- unique(control1)
  #make sure there are no affected ids within the control group
  non_overlap1 <- dplyr::anti_join(control1, affectedData, by = c("temp1" = "new_index"))
  
  #KS test
  ks1 <- ks.test(non_overlap1, affectedData$new_index)
  #p-value = 1, fail to reject null hpyothesis that the two samples are not significantly different
  print(ks1)
}

# this attaches all data because above just gets ids for sample 
group1 <- select(filter(newdata, new_index %in% non_overlap1$temp), c(1:65))


#Control Group 2
ks2 = 0
ks2$p.value = 0
while(ks2$p.value < 0.2) {
  #print("here")
  temp2 = vector()
  for (i in affectedData$new_index) {
    #print(i)
    # y here is the row number of affected ids in the full dataset
    y2 = which(newdata$new_index == i) 
    #print(y)
    temp2 = append(temp2, newdata[sample((y2-250):(y2+250), 50, replace = FALSE), "new_index"])
  }
  temp2 <- sort(temp2)
  control2 <- as.data.frame(temp2)
  #make sure there are no duplicate control values
  control2 <- unique(control2)
  #make sure there are no affected ids within the control group
  non_overlap2 <- dplyr::anti_join(control2, affectedData, by = c("temp2" = "new_index"))
  
  #KS test
  ks2 <- ks.test(non_overlap2, affectedData$new_index)
  #p-value = 1, fail to reject null hpyothesis that the two samples are not significantly different
  print(ks2)
}

# this attaches all data because above just gets ids for sample 
group2 <- select(filter(newdata, new_index %in% non_overlap2$temp), c(1:65))

# make sure to have two different control groups
library(dplyr)
group2 <- anti_join(group1, group2, by = "new_index")

#use group1 to impute Affected NAs then pull back out
# need to first get both datasets to have matching types
affectedData[,c(5:65)]<- sapply(affectedData[,5:65],as.numeric)

affectedData$gender = as.factor(affectedData$gender)

affectedData$Affected = as.factor(affectedData$Affected)

library(gtools)
# need to add affected data with NAs back in to impute w/ group 1
imputeData <- smartbind(group1, affectedData)
#sort so that affected data is integrated within the control data
imputeData <- arrange(imputeData, birth_year, new_index)


library(mice)
# check pattern of missing values in data
md.pattern(imputeData)

library(VIM)
# view plot of data pattern for missing values
#shows 7 variables with any missing data in affected data
aggr_plot <- aggr(imputeData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(imputeData), cex.axis=.7, gap=3,
                  ylab=c("Histogram of missing data","Pattern"))

# impute data to fill missing values using 'cart' method to account for categorical variables
# lso, attempted using 'rf' (random forest) method Note: default method 'pmm' does not work 
# Info found at: https://www.rdocumentation.org/packages/mice/versions/3.3.0/topics/mice
# Step-by-step: https://www.youtube.com/watch?v=zX-pacwVyvU
# tempData <- mice(imputeData,m=5,maxit=5,meth='cart',seed=500)
# logged errors on 6 variables (all observations too similar) 
#BRINGS ISSUE 6 variables have too similar of data for every record in prediciton
# > head(tempData$loggedEvents, 10)
# it im         dep meth                                                out
# 1   1  1         ASA cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1
# 2   1  1      C0.C16 cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1
# 3   1  1      C0.C18 cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1
# 4   1  1 C14.1.C12.1 cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1
# 5   1  1   C5.DC.C16 cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1
# 6   1  1    C5.DC.C8 cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1
# 7   1  1     Leu.Ala cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1
# 8   1  2         ASA cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1
# 9   1  2      C0.C16 cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1
# 10  1  2      C0.C18 cart C14.OH, C16.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1

# trial to remove 6 variables imputation doesn't like 
trialData <- subset(imputeData, select = -c(18, 23, 24, 28, 30, 47))

# tempData <- mice(trialData,m=5,maxit=5,meth='cart',seed=500)
# rfData <- mice(trialData,m=5,maxit=5,meth='rf',seed=500)

# https://stefvanbuuren.name/mice/reference/mice.impute.rf.html
#random forest imputation of missing data
rfImpute <- mice(trialData, meth = 'rf', ntree = 10)
# no logged events after removing vars

# densityplot(tempData)
# densityplot(rfData)
densityplot(rfImpute)

# stripplot(tempData, pch = 20, cex = 1.2)
# stripplot(rfData, pch = 20, cex = 1.2)
stripplot(rfImpute, pch = 20, cex = 1.2)

# completedData <- complete(tempData, 1)
# completedDataRF <- complete(rfData, 3)
#pull one of the datasets from the generated imputations
completeDatarf <- complete(rfImpute, 3)

# add back 6 vars we didn't use to impute
#get them back from where we started
pBack <- subset(imputeData, select = c(18, 23, 24, 28, 30, 47))
#attach and reorder
full <- cbind(completeDatarf, pBack) %>%
  .[, c(1:17, 60, 18:21, 61, 62, 22:24, 63, 25, 64, 26:41, 65, 42:59)]
library(arsenal)
#check that the variables are in correct order
summary(compare(imputeData, full))

# pull out affected data to have a full data set based on imputed data
# put this full data back into entire dataset from earlier
newAffected <- select(filter(full, Affected == 1), c(1:65))
fullData <- select(filter(newdata, Affected == 0), c(1:65)) %>%
  dplyr::union(., newAffected) %>%
  arrange(., birth_year, new_index)

#******************
#***GOOD TO HERE***
#******************

#***Begin Modeling on group 2***
library(tidyverse)
data_nested <- group2 %>% 
  group_by(new_index) %>%
  nest()

data_unnested <- data_nested %>%
  unnest()

#***********************
#***IGNORE BELOW HERE***
#***********************
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
