# XGBTree 3.0

data = read.delim2('goodData.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
data = data[c(-1)]

library(dplyr)
#used later for imputing values (has missing data)
affectedData <- select(filter(data, Affected == 1), c(1:65))
#ratio for females to males in affected data
ratioAffected <- sum(affectedData$gender == "F")/sum(affectedData$gender == "M")
#ratioAffected = 0.33684210

library(dplyr)
# gets rid of two unneeded categories for gender
df <- select(filter(data, gender != "U" & gender != ""), c(1:65))
# imputation is done later with one control group (group1) and all affected observations

# Use for fixing ratio without losing affected data
nonAffectedData <- select(filter(data, Affected == 0), c(1:65))

#this is adjusting the gender ratio to ensure there is no bias between controls and actual affected ratios
fixRatio <- df[sample( which(df$gender=='F'), round(0.35265*length(which(df$gender=='F')))), ]

justmale <- select(filter(df, gender == "M"), c(1:65))

newData <- dplyr::union(justmale, fixRatio)

#check ratio
sum(newData$gender == "F") / sum(newData$gender == "M")
#0.336839

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
sum(is.na(validateData))

# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/

library(caret)
#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(trainData, method = c("knnImpute","center","scale"))
preProcValues2 <- preProcess(validateData, method = c("knnImpute","center","scale"))

#This area is where the scaling occurs ^^^

library('RANN')
train_processed <- predict(preProcValues, trainData)
sum(is.na(train_processed))
str(train_processed)

validate_processed <- predict(preProcValues2, validateData)
sum(is.na(validate_processed))
str(validate_processed)

#Converting outcome variable to numeric
train_processed$Affected = as.numeric(train_processed$Affected)
validate_processed$Affected = as.numeric(validate_processed$Affected)

#Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))

dmy2 <- dummyVars(" ~ .", data = validate_processed,fullRank = T)
validate_transformed <- data.frame(predict(dmy2, newdata = validate_processed))

#Converting the dependent variable back to categorical
train_transformed$Affected <-as.factor(train_transformed$Affected)
str(train_transformed)

validate_transformed$Affected <-as.factor(validate_transformed$Affected)
str(validate_transformed)


####### Work Here ################
library(caret)
library(corrplot)			# plot correlations
library(doParallel)		# parallel processing
library(dplyr)        # Used by caret
library(gbm)				  # GBM Models
library(pROC)				  # plot the ROC curve
library(xgboost)      # Extreme Gradient Boosting

trainIndex <- createDataPartition(train_transformed$Affected,p=.7,list=FALSE)
trainData <- train_transformed[trainIndex,]
testData  <- train_transformed[-trainIndex,]
#
trainX <-trainData[,-4]        # Pull out the dependent variable
testX <- testData[,-4]
sapply(trainX,summary) 

ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

registerDoParallel(4,cores=4)
getDoParWorkers()

#all defaults right now
#low nrounds just to test
# 1 round takes : 
# does this time double per round????
xgb.grid <- expand.grid(nrounds = 1, 
                        eta = c(0.01, 0.1), 
                        max_depth = c(6),
                        min_child_weight = c(1),
                        gamma = c(0),
                        subsample = c(1),
                        colsample_bytree = c(1))

xgb.tune <-train(x=trainX,y=make.names(trainData$Affected),
                 method="xgbTree",
                 metric="ROC",
                 trControl=ctrl,
                 tuneGrid=xgb.grid)

#Need to run

xgb.tune$bestTune
plot(xgb.tune)  		# Plot the performance of the training models
res <- xgb.tune$results
res

### xgboostModel Predictions and Performance
# Make predictions using the test data set
xgb.pred <- predict(xgb.tune,testX)

#Look at the confusion matrix  
confusionMatrix(xgb.pred,testData$Affected)   

#Draw the ROC curve 
xgb.probs <- predict(xgb.tune,testX,type="prob")
#head(xgb.probs)

xgb.ROC <- roc(predictor=xgb.probs$PS,  #May be like X1
               response=testData$Affected,
               levels=rev(levels(testData$Affected)))
xgb.ROC$auc
# Area under the curve: 0.8857

plot(xgb.ROC,main="xgboost ROC")
# Plot the propability of poor segmentation
histogram(~xgb.probs$PS|testData$Affected,xlab="Probability of Poor Segmentation") # PS = like X1



#

# library(readxl)
# library(tidyverse)
# library(xgboost)
# library(caret)
# # Create index for testing and training data
# inTrain <- createDataPartition(y = train_transformed$Affected, p = 0.8, list = FALSE)
# # subset power_plant data to training
# training <- train_transformed[inTrain,]
# # subset the rest to test
# testing <- train_transformed[-inTrain,]
# 
# 
# X_train = xgb.DMatrix(as.matrix(training %>% select(-Affected)))
# y_train = training$Affected
# X_test = xgb.DMatrix(as.matrix(testing %>% select(-Affected)))
# y_test = testing$Affected
# 
# xgb_trcontrol = trainControl(
#   method = "cv",
#   number = 5,  
#   allowParallel = TRUE,
#   verboseIter = FALSE,
#   returnData = FALSE
# )
# 
# xgbGrid <- expand.grid(nrounds = c(100,200),  
#                        max_depth = c(10, 15, 20, 25),
#                        colsample_bytree = seq(0.5, 0.9, length.out = 5),
#                        ## The values below are default values in the sklearn-api. 
#                        eta = 0.1,
#                        gamma=0,
#                        min_child_weight = 1,
#                        subsample = 1
# )
# 
# xgb_model = train(
#   X_train, y_train,  
#   trControl = xgb_trcontrol,
#   tuneGrid = xgbGrid,
#   method = "xgbTree"
# )
# 
# xgb_model$bestTune
# 
# 
# xgb_model$bestTune
# plot(xgb_model)
# 
# res <- xgb_model$results
# res
# 
# 
# 
# xgb.probs <- predict(xgb_model,X_test)
# head(xgb.probs)
# 
# confusionMatrix(xgb.probs,testing$Affected)   
# 
# library(pROC)
# xgb.ROC <- roc(predictor=xgb.probs$`1`,
#                response=train$Affected,
#                levels=rev(levels(train$Affected)))
# 
# xgb.ROC$auc
# plot(xgb.ROC,main="xgboost ROC")





# predicted = predict(xgb_model, X_test)
# residuals = as.numeric(y_test) - as.numeric(predicted)
# RMSE = sqrt(mean(residuals^2))
# cat('The root mean square error of the test data is ', round(RMSE,3),'\n')
# 
# y_test_mean = mean(as.numeric(y_test))
# # Calculate total sum of squares
# tss =  sum((as.numeric(y_test) - y_test_mean)^2 )
# # Calculate residual sum of squares
# rss =  sum(residuals^2)
# # Calculate R-squared
# rsq  =  1 - (rss/tss)
# cat('The R-square of the test data is ', round(rsq,3), '\n')
# 
# 
# options(repr.plot.width=8, repr.plot.height=4)
# my_data = as.data.frame(cbind(predicted = predicted,
#                               observed = y_test))
# # Plot predictions vs test data
# ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
#   geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
#   xlab("Status") + ylab("Affected") + 
#   theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
#         axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
#         axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))




