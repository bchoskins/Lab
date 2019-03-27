# Normalize Training Data Random Forest

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

library(caTools)
sample = sample.split(train_transformed,SplitRatio = 0.75) 
train1 =subset(train_transformed,sample ==TRUE) 
test1=subset(train_transformed, sample==FALSE)


prop.table(table(train1$Affected))

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     search = "random",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

orig_fit <- caret::train(make.names(Affected) ~ .,
                  data = train1,
                  method = "rf",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl,
                  tuneLength = 30)

test_roc <- function(model, data) {
  
  roc(data$Affected,
      predict(model, data, type = "prob")[, "X2"])
  
}

library(pROC)
orig_fit %>%
  test_roc(data = test1) %>%
  auc()


model_weights <- ifelse(train1$Affected == 1,
                        (1/table(train1$Affected)[1]) * 0.5,
                        (1/table(train1$Affected)[2]) * 0.5)

ctrl$seeds <- orig_fit$control$seeds

weighted_fit <- caret::train(make.names(Affected) ~ .,
                      data = train1,
                      method = "rf",
                      verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)


ctrl$sampling <- "down"

down_fit <- caret::train(make.names(Affected) ~ .,
                  data = train1,
                  method = "rf",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl,
                  tuneLength = 30)

#takes a bit
ctrl$sampling <- "up"

up_fit <- caret::train(make.names(Affected) ~ .,
                data = train1,
                method = "rf",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl,
                tuneLength = 30)

ctrl$sampling <- "smote"

smote_fit <- caret::train(make.names(Affected) ~ .,
                   data = train1,
                   method = "rf",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl,
                   tuneLength = 30)


model_list <- list(orig = orig_fit,
                   down = down_fit,
                   up = up_fit,
                   SMOTE = smote_fit)

library(purrr)
library(pROC)
model_list_roc <- model_list %>%
  map(test_roc, data = test1)

library(dplyr)
model_list_roc %>%
  map(auc)

# $orig
# Area under the curve: 0.6044
# $down
# Area under the curve: 0.5419
# 
# $up
# Area under the curve: 0.543
# 
# $SMOTE
# Area under the curve: 0.5625


#With 15 sampled per affect 
# $orig
# Area under the curve: 0.5201
# 
# $down
# Area under the curve: 0.5013
# 
# $up
# Area under the curve: 0.469
# 
# $SMOTE
# Area under the curve: 0.5272

results_list_roc <- list(NA)
num_mod <- 1

library(dplyr)
for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

custom_col <- c("#00777C", "#0072B2", "#D55E00", "#CC79A7")

library(ggplot2)
ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

library(caret)
varImp(orig_fit)

library(pROC)
orig_fit %>%
  test_roc(data = test1) %>%
  auc()

#New Data 
# Area under the curve: 0.5625 SMOTE
# Area under the curve: 0.6044 ORIG

# Overall
# C8.1         100.00
# C8.C10        88.68
# C5.DC.C16     73.54
# Cit           63.29
# Leu.Phe       56.25
# Arg           55.81
# C0.C16        54.93
# C14.1         53.77
# C4.C3         53.61
# Phe           53.32
# Glu           51.34
# C.12          50.71
# C5            49.75
# Cit.Arg       46.87
# C0.C18        39.10
# Tyr           38.70
# C5.OH         37.65
# C4.DC         34.20
# new_index     31.85
# Asp.Homocit   26.31

# > mean(model_list_roc$up$sensitivities)
# [1] 0.8782609
# > mean(model_list_roc$up$specificities)
# [1] 0.1769231

#NEW SMOTE
# mean(model_list_roc$SMOTE$sensitivities
#      + )
# [1] 0.5081049
# > mean(model_list_roc$SMOTE$specificities)
# [1] 0.5336538

#NEW ORIG
# > mean(model_list_roc$orig$sensitivities)
# [1] 0.901308
# > mean(model_list_roc$orig$specificities)
# [1] 0.1649572

# distancelist <- list()
# for (i in model_list_roc$up$sensitivities) {
#   #print(i)
#   for (j in 1-model_list_roc$up$specificities){
#     #print(j)
#     c = sqrt((j-0)^2+(i-1)^2)
#     print(c)
#     print("here")
#     # dist <- paste('distance:',sep='')
#     # tmp <- list(val = c)
#     distancelist <- c(c)
#   }
# }


dist <- list()
for ( i in  1:length(model_list_roc$orig$sensitivities)) {
   c = sqrt(((1-model_list_roc$orig$specificities[i])-0)^2 + (model_list_roc$orig$sensitivities[i]-1)^2)
   dist[[paste0("distance", i)]] <- c
}

print(min(dist))


#NEW ORIG
# > Reduce(min, dist)
# [1] 0.599197

# > which(dist==Reduce(min,dist))
# distance110 
# 110 

# model_list_roc$orig$sensitivities[110]
# [1] 0.5172414

# model_list_roc$orig$specificities[110]
# [1] 0.6450617

##NEW SMOTE
# > Reduce(min, dist)
# [1] 0.5916232

# which(dist==Reduce(min,dist))
# distance118 
# 118 
# 
# model_list_roc$SMOTE$sensitivities[118]
# [1] 0.5862069
# 
# model_list_roc$SMOTE$specificities[118]
# [1] 0.5771605



##OLD UP
# > Reduce(min, dist)
# [1] 0.5521185
# > which(dist==Reduce(min,dist))
# distance108 
# 108 

# > model_list_roc$up$sensitivities[108]
# [1] 0.7272727

# > model_list_roc$up$specificities[108]
# [1] 0.519943

# > sum(is.na(data$C8.1))
# [1] 3
# > sum(is.na(data$C8.C10))
# [1] 2
# > sum(is.na(data$Cit))
# [1] 2
# > sum(is.na(data$Leu.Phe))
# [1] 2
# > sum(is.na(data$C5.DC.C16))
# [1] 34909
# > sum(is.na(data$Arg))
# [1] 3



