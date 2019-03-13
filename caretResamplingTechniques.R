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

#############################################

dt = sort(sample(nrow(train_transformed), nrow(train_transformed)*.75))
train<-train_transformed[dt,]
test<-train_transformed[-dt,]

table(train$Affected)

#### inside resampling (done in trainControl) #####

down_ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

down_inside <- caret::train(make.names(Affected) ~ ., data = train,
                     method = "treebag",
                     nbagg = 50,
                     metric = "ROC",
                     trControl = down_ctrl)

up_ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary,
                          sampling = "up")

up_inside <- caret::train(make.names(Affected) ~ ., data = train,
                   method = "treebag",
                   nbagg = 50,
                   metric = "ROC",
                   trControl = up_ctrl)

rose_ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        sampling = "rose")

rose_inside <- caret::train(make.names(Affected) ~ ., data = train,
                     method = "treebag",
                     nbagg = 50,
                     metric = "ROC",
                     trControl = rose_ctrl)


smote_ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        sampling = "smote")

smote_inside <- caret::train(make.names(Affected) ~ ., data = train,
                      method = "treebag",
                      nbagg = 50,
                      metric = "ROC",
                      trControl = smote_ctrl)




smoteProbs <- predict(smote_inside, test, type = "prob")[,1]

smotesampledROC <- roc(response = test$Affected, 
                      predictor = smoteProbs,
                      levels = rev(levels(test$Affected)))

plot(smotesampledROC, col = rgb(1, 0, 0, .5), lwd = 2)

getTrainPerf(smote_inside)

auc(smotesampledROC)



# Down sampled in train control
# > getTrainPerf(down_inside)
# TrainROC TrainSens TrainSpec  method
# 1 0.4805706 0.4930904 0.4633333 treebag
# > auc(downsampledROC)
# Area under the curve: 0.5779

# Up sampled in train control
# getTrainPerf(up_inside)
# TrainROC TrainSens TrainSpec  method
# 1 0.415285 0.9953854         0 treebag
# > auc(upsampledROC)
# Area under the curve: 0.577

# SMOTE sampled in train control
# getTrainPerf(smote_inside)
# TrainROC TrainSens  TrainSpec  method
# 1 0.4679154 0.8824727 0.09266667 treebag
# > auc(smotesampledROC)
# Area under the curve: 0.5851

# ROSE sampled in train control
# > getTrainPerf(rose_inside)
# TrainROC TrainSens TrainSpec  method
# 1 0.3920078 0.6624225 0.2273333 treebag
# > auc(rosesampledROC)
# Area under the curve: 0.6429











# inside_models <- list(down = down_inside,
#                       up = up_inside,
#                       SMOTE = smote_inside,
#                       ROSE = rose_inside)
# 
# inside_resampling <- resamples(inside_models)
# 
# inside_test <- lapply(inside_models, test_roc, data = imbal_test)
# inside_test <- lapply(inside_test, as.vector)
# inside_test <- do.call("rbind", inside_test)
# colnames(inside_test) <- c("lower", "ROC", "upper")
# inside_test <- as.data.frame(inside_test)
# 
# summary(inside_resampling, metric = "ROC")





#





# # Down sampled
# down_train <- downSample(x = train[, -ncol(train)],
#                          y = train$Affected)
# table(down_train$Affected)
# down_train <- down_train[,-c(65)]
# 
# # Up sampled
# up_train <- upSample(x = train[, -ncol(train)],
#                      y = train$Affected)
# table(up_train$Affected)
# 
# 
# #testing SMOTE algorithm
# library(DMwR)
# 
# smote_train <- SMOTE(Affected ~ ., data  = train)
# table(smote_train$Affected)
# 
# #testing with ROSE algorithm
# library(ROSE)
# 
# rose_train <- ROSE(Affected ~ ., data  = train)$data
# table(rose_train$Affected)
# 
# 
# ctrl <- trainControl(method = "repeatedcv", repeats = 5,
#                      classProbs = TRUE,
#                      summaryFunction = twoClassSummary)
# 
# #orignal model
# orig_fit <- caret::train(make.names(Affected) ~ ., data = train,
#                   method = "treebag",
#                   nbagg = 50,
#                   metric = "ROC",
#                   trControl = ctrl)
# #down sampled model
# down_outside <- caret::train(make.names(Affected) ~ ., data = down_train,
#                       method = "treebag",
#                       nbagg = 50,
#                       metric = "ROC",
#                       trControl = ctrl)
# #up sampled model
# up_outside <- caret::train(make.names(Affected) ~ ., data = up_train,
#                     method = "treebag",
#                     nbagg = 50,
#                     metric = "ROC",
#                     trControl = ctrl)
# 
# #ROSE model
# rose_outside <- caret::train(make.names(Affected) ~ ., data = rose_train,
#                       method = "treebag",
#                       nbagg = 50,
#                       metric = "ROC",
#                       trControl = ctrl)
# 
# 
# #SMOTE model
# smote_outside <- caret::train(make.names(Affected) ~ ., data = smote_train,
#                        method = "treebag",
#                        nbagg = 50,
#                        metric = "ROC",
#                        trControl = ctrl)

