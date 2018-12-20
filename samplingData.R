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
# md.pattern(imputeData)

library(VIM)
# view plot of data pattern for missing values
#shows 7 variables with any missing data in affected data
# aggr_plot <- aggr(imputeData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
#                   labels=names(imputeData), cex.axis=.7, gap=3,
#                   ylab=c("Histogram of missing data","Pattern"))

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
# stripplot(rfImpute, pch = 20, cex = 1.2)

# completedData <- complete(tempData, 1)
# completedDataRF <- complete(rfData, 3)
#pull one of the datasets from the generated imputations
completeDatarf <- mice::complete(rfImpute, 3)

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

# add affecteds back into group 2
modelData <- dplyr::union(group2, newAffected) %>%
  arrange(., birth_year, new_index)
# fix warning by changing from character back to factor

modelData$Affected <- as.factor(modelData$Affected)

#******************
#***GOOD TO HERE***
#******************

# Splitting the data into training set and test set
library(caTools)
set.seed(123)
split = sample.split(modelData$Affected, SplitRatio = 0.75)
training_set = subset(modelData, split == TRUE)
test_set = subset(modelData, split == FALSE)

# Feature scaling (since things like birth_year and new_index vary 
# greatly from other continuous variables) ???????? 
training_set[,c(1,3,5:65)] = scale(training_set[,c(1,3,5:65)])
test_set[,c(1,3,5:65)] = scale(test_set[,c(1,3,5:65)])

#Fitting Logistic Regression to the Training Set
# classifier = glm(formula = Affected ~ .,
#                  family = "binomial",
#                  data = training_set,
#                  maxit = 50)

# library(logistf)
# classifier = logistf(Affected ~ ., data = training_set)


# library(brglm)
# classifier.brglm = brglm(Affected ~ + ., family = binomial(logit),
#                          data = training_set,
#                          method = "brglm.fit",
#                          control.brglm=brglm.control(br.maxit=1000))
# 
# summary(classifier.brglm)

library(arm)
classifier.bayesglm = bayesglm(Affected ~ ., family = binomial(link="logit"),
                               data = training_set)

summary(classifier.bayesglm)

# Predicting test set results 
prob_pred = predict(classifier.bayesglm, type = 'response', newdata = test_set[-4])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
# pretty poor classifier but we'll try better later
cm = table(test_set[,4], y_pred)












#****BELOW HERE IS DATA CAMP LOGISTIC REGRESION****

# library(rsample)
# set.seed(42)
# 
# #prepare the intial split object
# data_split <- initial_split(modelData, prop=0.75)
# # Extract the training dataframe 
# training_data <- training(data_split)
# # Extract the testing data frame
# testing_data <- testing(data_split)
# 
# set.seed(42)
# cv_split <- vfold_cv(training_data, v =5)
# 
# library(tidyverse)
# cv_data <- cv_split %>%
#   mutate(
#     train = map(splits, ~training(.x)),
#     validate = map(splits, ~testing(.x))
#   )
# 
# # Build a model using the train data for each fold 
# # of the cross validation
# cv_models_lr <- cv_data %>%
#   mutate(model = map(train, ~glm(formula = Affected~., data = .x, 
#                                  family = "binomial")))
# 
# # Extract the first model and validate 
# model <- cv_models_lr$model[[1]]
# validate <- cv_models_lr$validate[[1]]
# 
# # Prepare binary vector of actual Attrition values in validate
# validate_actual <- validate$Affected == "Yes"
# 
# # Predict the probabilities for the observations in validate
# validate_prob <- predict(model, validate, type = "response")
# 
# # Prepare binary vector of predicted Attrition values for validate
# validate_predicted <- validate_prob > 0.5
# 
# library(Metrics)
# 
# # Compare the actual & predicted performance visually using a table
# table(validate_actual, validate_predicted)
# 
# # Calculate the accuracy
# accuracy(validate_actual, validate_predicted)
# 
# # Calculate the precision
# precision(validate_actual, validate_predicted)
# 
# # Calculate the recall
# recall(validate_actual, validate_predicted)
# 
# cv_prep_lr <- cv_models_lr %>% 
#   mutate(
#     # Prepare binary vector of actual Attrition values in validate
#     validate_actual = map(validate, ~.x$Affected == "Yes"),
#     # Prepare binary vector of predicted Attrition values for validate
#     validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y, type = "response") > 0.5)
#   )
# 
# # Calculate the validate recall for each cross validation fold
# cv_perf_recall <- cv_prep_lr %>% 
#   mutate(validate_recall = map2_dbl(validate_actual, validate_predicted, 
#                                     ~recall(actual = .x, predicted = .y)))
# 
# # Print the validate_recall column
# cv_perf_recall$validate_recall
# 
# # Calculate the average of the validate_recall column
# mean(cv_perf_recall$validate_recall)


