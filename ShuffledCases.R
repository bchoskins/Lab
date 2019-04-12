# Shuffled Cases AUC

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

#### Maybe work here on normalizing


sum(is.na(trainData))
sum(is.na(validateData))

library(caret)
#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(trainData, method = c("knnImpute","center","scale"))
preProcValues2 <- preProcess(validateData, method = c("knnImpute","center","scale"))

#This area is where the scaling occurs

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


##### Now testing random forest

## Mayne feature selection would make a difference????

# library(caTools)
# sample = sample.split(train_transformed,SplitRatio = 0.75) 
# train1 =subset(train_transformed,sample ==TRUE) 
# test1=subset(train_transformed, sample==FALSE)

####Shuffle cases here then model
#train1$Affected = sample(train1$Affected, replace=FALSE)
df2 <- transform(train_transformed, Affected = sample(Affected) )

prop.table(table(df2$Affected))

# ctrl <- trainControl(method = "repeatedcv",
#                      number = 5,
#                      repeats = 2,
#                      search = "random",
#                      summaryFunction = twoClassSummary,
#                      classProbs = TRUE)
ctrl <- trainControl(method ="cv",
                     classProbs = TRUE,
                     search = "random",
                     summaryFunction = twoClassSummary)



for (i in 1:5) {
  shuffle_fit <- caret::train(make.names(Affected) ~ .,
                           data = df2,
                           method = "rf",
                           verbose = FALSE,
                           metric = "ROC",
                           trControl = ctrl,
                           tuneLength = 20)
  name = paste("/wdata/rotating_students/bhoskins/Lab_BH/shuffle", i, sep="_")
  save(shuffle_fit, file = name)
}

rf.roc <- roc(train_transformed$Affected, orig_fit$finalModel$votes[,"X2"])

#Wrong
# > orig_fit$results$ROC
# [1] 0.5080311 0.5199544 0.5131365 0.5203264 0.5247938 0.5212871 0.5153858 0.5187287 0.5207610

test_roc <- function(model, data) {
  
  roc(data$Affected,
      predict(model, data, type = "prob")[, "X2"])
  
}

library(pROC)
orig_fit %>%
  test_roc(data = test1) %>%
  auc()

#

# ctrl <- trainControl(method = "repeatedcv",
#                      number = 10,
#                      repeats = 5,
#                      search = "random",
#                      summaryFunction = twoClassSummary,
#                      classProbs = TRUE)
# 
# 
# orig_fit <- caret::train(make.names(Affected) ~ .,
#                          data = df2,
#                          method = "rf",
#                          verbose = FALSE,
#                          metric = "ROC",
#                          trControl = ctrl)
#Area under the curve: 0.5687

# ctrl <- trainControl(method = "repeatedcv",
#                      number = 5,
#                      repeats = 2,
#                      search = "random",
#                      summaryFunction = twoClassSummary,
#                      classProbs = TRUE)
# 
# 
# orig_fit <- caret::train(make.names(Affected) ~ .,
#                          data = df2,
#                          method = "rf",
#                          verbose = FALSE,
#                          metric = "ROC",
#                          trControl = ctrl,
#                          tuneLentgh = 10)



load("shuffle_2")
load("shuffle_3")
attach("shuffle_4")
