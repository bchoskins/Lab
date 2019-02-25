#Sample data/Imputation of missing affected data/construct models from sampled data (TESTING WITH CARET PACKAGE)
#NOTE: testing imputation early takes too long, need to impute after sampling

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
group2 <- setdiff(group2, group1, by = "new_index")

#confirm no overlapping observations
same <- dplyr::intersect(group1, group2)

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
str(train_transformed)


####Trying out several tuning methods in caret####

library(randomForest)
library(mlbench)
library(caret)

x <- train_transformed[,-c(4)]
y <- train_transformed[,c(4)]

# Create model with default paramters
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# seed <- 7
# metric <- "Accuracy"
# set.seed(seed)
# mtry <- sqrt(ncol(x))
# tunegrid <- expand.grid(.mtry=mtry)
# #takes a couple min to run
# rf_default <- train(Affected~., data=train_transformed, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
# print(rf_default)



# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
# takes too long
rf_random <- train(Affected~., data=train_transformed, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


# ####Trying out stratified sampling in model####
# model <- randomForest(Affected ~., data=train_transformed, sampsize=c(69,69), strata=train_transformed$Affected, ntree=2000, mtry=8)
# summary(model)
# model
# varImpPlot(model)
# 
# # #results
# # Type of random forest: classification
# # Number of trees: 125
# # No. of variables tried at each split: 8
# # 
# # OOB estimate of  error rate: 11.45%
# # Confusion matrix:
# #   1   2 class.error
# # 1 2421 252  0.09427609
# # 2   62   7  0.89855072
# 
# ####Working on tuning parameters####
# modelLookup(model='rf')
# 
# #mtry of 6, 8 are best OOB
# x <- train_transformed[,-c(4)]
# y <- train_transformed[,c(4)]
# 
# 
# # Manual Search
# seed <- 7
# metric <- "Accuracy"
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
# modellist <- list()
# for (ntree in c(1000, 1500, 2000, 2500)) {
#   set.seed(seed)
#   fit <- train(Affected~., data=train_transformed, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
#   key <- toString(ntree)
#   modellist[[key]] <- fit
# }
# # compare results
# results <- resamples(modellist)
# summary(results)
# dotplot(results)





# set.seed(7)
# bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
# print(bestmtry)
# tunegrid <- expand.grid(.mtry=8)
# # mtry <- sqrt(ncol(x))
# # tunegrid <- expand.grid(.mtry=mtry)
# model <- randomForest(Affected ~., data=train_transformed, sampsize=c(69,69), strata=train_transformed$Affected, ntree=500, tuneGrid=tunegrid)
# summary(model)
# model
# varImpPlot(model)


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




