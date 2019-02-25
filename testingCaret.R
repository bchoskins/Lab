#testing caret package on previous work starting with imputation of control groups

#Sample data/Imputation of missing affected data/construct models from sampled data

data = read.delim2('goodData.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
data = data[c(-1)]

library(dplyr)
#used later for imputing values (has missing data)
affectedData <- select(filter(data, Affected == 1), c(1:65))
#ratio for females to males in affected data
ratioAffected <- sum(affectedData$gender == "F")/sum(affectedData$gender == "M")
#ratioAffected = 0.4081633

library(plyr)
# gets rid of two unneeded categories for gender
df <- select(filter(data, gender != "U" & gender != ""), c(1:65))
# imputation is done later with one control group (group1) and all affected observations

# Use for fixing ratio without losing affected data
nonAffectedData <- select(filter(data, Affected == 0), c(1:65))

#this is adjusting the gender ratio to ensure there is no bias between controls and actual affected ratios
fixRatio <- df[sample( which(df$gender=='F'), round(0.427273849*length(which(df$gender=='F')))), ]

justmale <- select(filter(df, gender == "M"), c(1:65))
newData <- dplyr::union(justmale, fixRatio)

#check ratio
sum(newData$gender == "F") / sum(newData$gender == "M")
# 0.4081158

#check the number of males and females in the unaffected data so we can adjust ratio
#sum(genderCheck$gender == "M") # = 220230
#sum(genderCheck$gender == "F") # = 210070

# #take a just male dataset that can remain unchanged since affecteds are male dominent
# justmale <- select(filter(genderCheck, gender == "M"), c(1:65))
# #adjust the number of female observations to more accurately depict the ratio of F:M from affecteds
# fixFemale <- genderCheck[sample( which(genderCheck$gender=='F'), round(0.427905*length(which(genderCheck$gender=='F')))), ]
# 
# newratio <- sum(fixFemale$gender == "F")/sum(justmale$gender == "M")
# #new ratio = 0.40816419
# 
# joingender <- dplyr::union(justmale, fixFemale)
# 
# newdata <- dplyr::union(joingender, affectedData) 
# newdata <- arrange(newdata, birth_year, new_index)
# 
# # sum(newdata$gender =="F")/sum(newdata$gender == "M")
# # 0.4081642
# # sum(affectedData$gender == "F")/sum(affectedData$gender == "M")
# # 0.4081633

fixedData <- dplyr::union(newData, affectedData) %>%
  arrange(., birth_year, new_index)

# changing of variable types
fixedData[,c(5:65)]<- sapply(fixedData[,5:65],as.numeric)

fixedData$gender = as.factor(fixedData$gender)

fixedData$Affected = as.factor(fixedData$Affected)

#Pull two 50 observation control groups based on distance from each affected id
#and store into a list

#while loop to make sure we pass K-S Test of ***Making sure 'the two samples were drawn from the same distribution"***
# this control group is used for imputing missing affected data
#Control group 1
ks1 = 0
ks1$p.value = 0
while(ks1$p.value < 0.2) {
  temp1 = vector()
  for (i in affectedData$new_index) {
    # y here is the row number of affected ids in the full dataset
    y1 = which(fixedData$new_index == i) 
    #print(y)
    temp1 = append(temp1, fixedData[sample((y1-150):(y1+150), 50, replace = FALSE), "new_index"])
  }
  temp1 <- sort(temp1)
  control1 <- as.data.frame(temp1)
  #make sure there are no duplicate control values
  control1 <- unique(control1)
  #make sure there are no affected ids that got sampled within the control group (adding them later)
  non_overlap1 <- dplyr::anti_join(control1, affectedData, by = c("temp1" = "new_index"))
  
  #K-S test
  ks1 <- ks.test(non_overlap1, affectedData$new_index)
  #want p-value > 0.2 to fail to reject null hypothesis that the data are following a similar distribution as the
  # affected data throughout
  print(ks1)
}

# this attaches all data because above just gets ids for sample 
group1 <- select(filter(fixedData, new_index %in% non_overlap1$temp), c(1:65))


#repeat above for second control group used for modeling
#Control Group 2
ks2 = 0
ks2$p.value = 0
while(ks2$p.value < 0.2) {
  temp2 = vector()
  for (i in affectedData$new_index) {
    # y here is the row number of affected ids in the full dataset
    y2 = which(fixedData$new_index == i) 
    #print(y)
    temp2 = append(temp2, fixedData[sample((y2-150):(y2+150), 50, replace = FALSE), "new_index"])
  }
  temp2 <- sort(temp2)
  control2 <- as.data.frame(temp2)
  #make sure there are no duplicate control values
  control2 <- unique(control2)
  #make sure there are no affected ids within the control group
  non_overlap2 <- dplyr::anti_join(control2, affectedData, by = c("temp2" = "new_index"))
  
  #K-S test
  ks2 <- ks.test(non_overlap2, affectedData$new_index)
  #want p-value < 0.2 to reject null hypothesis that distributions are not significantally different
  print(ks2)
}

# this attaches all data because above just gets ids for sample 
group2 <- select(filter(fixedData, new_index %in% non_overlap2$temp), c(1:65))

# make sure to have two entirely different control groups
library(dplyr)
group2 <- anti_join(group1, group2, by = "new_index")

###GOOD FOR SAMPLING CONTROL GROUPS####

# use group1 to impute Affected and group 1 NAs
# need to first get both datasets to have matching types
affectedData[,c(5:65)]<- sapply(affectedData[,5:65],as.numeric)

affectedData$gender = as.factor(affectedData$gender)

affectedData$Affected = as.factor(affectedData$Affected)

library(gtools)
# need to add affected data with NAs back in to impute w/ group 1
validateData <- smartbind(group1, affectedData)
#sort so that affected data is integrated within the control data
validateData <- arrange(validateData, birth_year, new_index)

library(gtools)
# need to add affected data with NAs back in to impute w/ group 2
trainData <- smartbind(group2, affectedData)
#sort so that affected data is integrated within the control data
trainData <- arrange(trainData, birth_year, new_index)


#***GOOD TO HERE***
#******************

sum(is.na(trainData))

library(caret)
#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(trainData, method = c("knnImpute","center","scale"))

library('RANN')
train_processed <- predict(preProcValues, trainData)
sum(is.na(train_processed))
str(train_processed)

#Converting outcome variable to numeric
train_processed$Affected = as.numeric(train_processed$Affected)

#Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))

#Converting the dependent variable back to categorical
train_transformed$Affected <-as.factor(train_transformed$Affected)


#Trying out stratified sampling in model
model <- randomForest(Affected ~., data=train_transformed, sampsize=c(69,69), strata=train_transformed$Affected)
summary(model)
model
varImpPlot(model)
#

# outcomeName<-'Affected'
# predictors<-names(train_transformed)[!names(train_transformed) %in% outcomeName]
# 
# library(randomForest)
# model1 <- randomForest(Affected~ ., data = train_transformed, importance = TRUE)
# summary(model1)
# 
# 
# # Fine tuning parameters of Random Forest model
# model2 <- randomForest(Affected ~ ., data = train_transformed, ntree = 500, mtry = 6, importance = TRUE)
# model2
# summary(model2)

# Predicting on train set
predTrain <- predict(model, train_transformed, type = "class")
# Checking classification accuracy
table(predTrain, train_transformed$Affected)  






# model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
# 
# model_rf<-train(train_transformed[,predictors],train_transformed[,outcomeName],method='rf')
# 
# model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
# model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')


