# XGBTree in caret 2.0

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


# ratioed smapling needs to be done here
# here we adjust the the affecteds in each, do about 60% in validate and 40% in training so that they do not overlap. 
# require(caTools)
# set.seed(123)   #  set seed to ensure you always have same random numbers generated
# sample = sample.split(affectedData,SplitRatio = 0.60) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
# sampleV =subset(affectedData,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
# sampleT=subset(affectedData, sample==FALSE)
# 
# check <- dplyr::intersect(sampleT, sampleV)


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

library(caret)
#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(trainData, method = c("knnImpute","center","scale"))
preProcValues2 <- preProcess(validateData, method = c("knnImpute","center","scale"))


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


# ##### Up and Down sampling ######
# up_df <- upSample(x = train_transformed[, -ncol(train_transformed)],y = train_transformed$Affected)
# up_df <- as.data.frame(up_df)
# 
# trctrl <- trainControl(method = "cv", number = 5)
# 
# tune_grid <- expand.grid(nrounds=c(50, 100),
#                          max_depth = c(5:10),
#                          eta = c(0.001, 0.05),
#                          gamma = c(0.01, 1, 2),
#                          colsample_bytree = c(0.75, 1.0),
#                          subsample = c(0.75),
#                          min_child_weight = c(0, 0.5, 1))
# 
# 
# 
# rf_fit <- caret::train(make.names(Affected) ~., data = up_df, method = "xgbTree",
#                 trControl=trctrl,
#                 tuneGrid = tune_grid,
#                 tuneLength = 10)
# 
# rf_fit
# 
# rf_fit$bestTune
# plot(rf_fit)
# 
# res <- rf_fit$results
# res
# 
# 
# 
# xgb.probs <- predict(rf_fit,type="prob")
# head(xgb.probs)
# 
# library(pROC)
# xgb.ROC <- roc(predictor=xgb.probs$`X1`,
#                response=up_df$Affected,
#                levels=rev(levels(up_df$Affected)))
# 
# xgb.ROC$auc
# plot(xgb.ROC,main="xgboost ROC")
# 
# 
# ###END####





# library(h2o)
# h2o.init()
# 
# y <- "Affected"
# x <- setdiff(names(train_transformed), y)
# 
# # Some XGboost/GBM hyperparameters
# hyper_params <- list(ntrees = seq(10, 1000, 50),
#                      learn_rate = seq(0.0001, 0.2, 0.0001),
#                      max_depth = seq(1, 20, 1),
#                      min_child_weight = seq(0, 1, 0.1),
#                      sample_rate = seq(0.5, 1.0, 0.0001),
#                      col_sample_rate = seq(0.2, 1.0, 0.0001))
# 
# search_criteria <- list(strategy = "RandomDiscrete",
#                         max_models = 10, 
#                         seed = 1)
# 
# # Train the grid
# xgb_grid <- h2o.grid(algorithm = "xgboost",
#                      x = x, y = y,
#                      training_frame = as.h2o(train_transformed),
#                      nfolds = 5,
#                      seed = 1,
#                      hyper_params = hyper_params,
#                      search_criteria = search_criteria)
# 
# 
# # Sort the grid by CV AUC
# grid <- h2o.getGrid(grid_id = xgb_grid@grid_id, sort_by = "AUC", decreasing = TRUE)
# grid_top_model <- grid@summary_table[1, "model_ids"]
# 
# h2o.shutdown()



####XGBTree Modeling####

#####re look at code from example, have to make adjustments to train and test sets

dt = sort(sample(nrow(train_transformed), nrow(train_transformed)*.75))
train<-train_transformed[dt,]
test<-train_transformed[-dt,]

table(train$Affected)

trctrl <- trainControl(method = "cv", number = 5)

tune_grid <- expand.grid(nrounds=c(50, 100),
                         max_depth = c(5:10),
                         eta = c(0.001, 0.05),
                         gamma = c(0.01, 1, 2),
                         colsample_bytree = c(0.75, 1.0),
                         subsample = c(0.75),
                         min_child_weight = c(0, 0.5, 1))


rf_fit <- caret::train(make.names(Affected) ~., data = train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

# rf_fit$bestTune
# plot(rf_fit)  		# Plot the performance of the training models
# res <- rf_fit$results
# res
# 
# ### xgboostModel Predictions and Performance
# # Make predictions using the test data set
# xgb.pred <- predict(rf_fit,test)
# 
# # #Look at the confusion matrix  
# # confusionMatrix(xgb.pred,testData$Class)   
# 
# #Draw the ROC curve 
# xgb.probs <- predict(rf_fit,test,type="prob")
# #head(xgb.probs)
# 
# xgb.ROC <- roc(predictor=xgb.probs$PS,
#                response=testData$Class,
#                levels=rev(levels(testData$Class)))
# xgb.ROC$auc
# # Area under the curve: 0.8857
# 
# plot(xgb.ROC,main="xgboost ROC")
# # Plot the propability of poor segmentation
# histogram(~xgb.probs$PS|testData$Class,xlab="Probability of Poor Segmentation")


# 
# rf_fit
# 
# rf_fit$bestTune
# plot(rf_fit)
# 
# res <- rf_fit$results
# res
# 
# 
# 
# xgb.probs <- predict(rf_fit,type="prob")
# head(xgb.probs)
# 
# library(pROC)
# xgb.ROC <- roc(predictor=xgb.probs$`1`,
#                response=train$Affected,
#                levels=rev(levels(train$Affected)))
# 
# xgb.ROC$auc
# plot(xgb.ROC,main="xgboost ROC")


#### Secondary XGBTree modleing ####
# 
# ## XGBOOST
# # Some stackexchange guidance for xgboost
# # http://stats.stackexchange.com/questions/171043/how-to-tune-hyperparameters-of-xgboost-trees
# 
# # Set up for parallel procerssing
# set.seed(1951)
# registerDoParallel(4,cores=4)
# getDoParWorkers()
# 
# # Train xgboost
# xgb.grid <- expand.grid(nrounds = 500, #the maximum number of iterations
#                         eta = c(0.01,0.1), # shrinkage
#                         max_depth = c(2,6,10))
# 
# xgb.tune <-train(x=trainX,y=trainData$Class,
#                  method="xgbTree",
#                  metric="ROC",
#                  trControl=ctrl,
#                  tuneGrid=xgb.grid)
# 
# 
# xgb.tune$bestTune
# plot(xgb.tune)  		# Plot the performance of the training models
# res <- xgb.tune$results
# res
# 
# ### xgboostModel Predictions and Performance
# # Make predictions using the test data set
# xgb.pred <- predict(xgb.tune,testX)
# 
# #Look at the confusion matrix  
# confusionMatrix(xgb.pred,testData$Class)   
# 
# #Draw the ROC curve 
# xgb.probs <- predict(xgb.tune,testX,type="prob")
# #head(xgb.probs)
# 
# xgb.ROC <- roc(predictor=xgb.probs$PS,
#                response=testData$Class,
#                levels=rev(levels(testData$Class)))
# xgb.ROC$auc
# # Area under the curve: 0.8857
# 
# plot(xgb.ROC,main="xgboost ROC")
# # Plot the propability of poor segmentation
# histogram(~xgb.probs$PS|testData$Class,xlab="Probability of Poor Segmentation")
