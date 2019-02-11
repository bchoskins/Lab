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

#while loop to make sure we pass K-S Test 
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
  #want p-value > 0.2 to fail to reject null hypothesis that the data are normally distrubuted based on distribution of 
  # affected data throughout
  print(ks1)
}

# this attaches all data because above just gets ids for sample 
group1 <- select(filter(fixedData, new_index %in% non_overlap1$temp), c(1:65))
#may need to run imputation on control group??? or just save for later

# #this is adjusting the gender ratio to ensure there is no bias between controls and actual affected ratios
# fixRatio <- group1[sample( which(group1$gender=='F'), round(0.4252*length(which(group1$gender=='F')))), ]
# justmale <- select(filter(group1, gender == "M"), c(1:65))
# group1 <- dplyr::union(justmale, fixRatio)
# 
# #check ratio
# sum(group1$gender == "F") / sum(group1$gender == "M")

#repeate above for second control group used for modeling
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

# #this is adjusting the gender ratio to ensure there is no bias between controls and actual affected ratios
# fixRatio2<- group2[sample( which(group2$gender=='F'), round(0.435*length(which(group2$gender=='F')))), ]
# justmale2 <- select(filter(group2, gender == "M"), c(1:65))
# group2 <- dplyr::union(justmale2, fixRatio2)
# 
# #check ratio
# sum(group2$gender == 'F')/ sum(group2$gender == 'M')

# make sure to have two entirely different control groups
library(dplyr)
group2 <- anti_join(group1, group2, by = "new_index")



###GOOD???####


# use group1 to impute Affected and group 1 NAs then pull affecteds back out
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
# aggr_plot <- aggr(imputeData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
#                   labels=names(imputeData), cex.axis=.7, gap=3,
#                   ylab=c("Histogram of missing data","Pattern"))

# impute data to fill missing values using 'cart' method to account for categorical variables (*did not do, now using 'rf' method)
# Also, attempted using 'rf' (random forest) method 
# Note: default method 'pmm' does not work 
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

# trial to remove 6 variables imputation doesn't like (C14.OH, C16.OH.C16, C18.OH, C18.1.OH, C5.1)
trialData <- subset(imputeData, select = -c(18,24,28,30,47))

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
#now contains control group 1 and affected data
completeDatarf <- mice::complete(rfImpute, 3)

# add back 6 vars we didn't use to impute
#get them back from where we started
# pBack <- subset(imputeData, select = c(18,24,28,30,42,47))
# 
# ####NEED TO FIX THIS REORDER#####
# 
# #attach and reorder
# full <- cbind(completeDatarf, pBack) %>%
#   .[, c(1:17, 60, 18:21, 61, 62, 22:24, 63, 25, 64, 26:41, 65, 42:59)]
# 
# library(arsenal)
# #check that the variables are in correct order
# summary(compare(imputeData, full))

# pull out affected data to have a full data set based on imputed data
# put this full data back into entire dataset from earlier (Also, will
# be used for modeling with control group 2)

newAffected <- select(filter(completeDatarf, Affected == 1), c(1:60))
# fullData <- select(filter(newdata, Affected == 0), c(1:65)) %>%
#   dplyr::union(., newAffected) %>%
#   arrange(., birth_year, new_index)

group2 <- subset(group2, select = -c(18,24,28,30,47))

###NEED TO IMPUTE GROUP2####
library(gtools)
# need to add affected data with NAs back in to impute w/ group 1
imputeData2 <- smartbind(group2, newAffected)
#sort so that affected data is integrated within the control data
imputeData2 <- arrange(imputeData2, birth_year, new_index)


library(mice)

rfImpute2 <- mice(imputeData2, meth = 'rf', ntree = 10)

densityplot(rfImpute2)

modelData<- mice::complete(rfImpute2, 3)

# modelData$Affected <- as.factor(modelData$Affected)
modelRatio <- sum(modelData$gender == "F") / sum(modelData$gender == "M")

#******************
#***GOOD TO HERE***
#******************


# Splitting the data into training set and test set
library(caTools)
set.seed(123)
split = sample.split(modelData$Affected, SplitRatio = 0.75)
training_set = subset(modelData, split == TRUE)
test_set = subset(modelData, split == FALSE)

#***Feature scaling (since things like birth_year and new_index vary 
# greatly from other continuous variables)*** ???????? 
# training_set[,c(1,3,5:65)] = scale(training_set[,c(1,3,5:65)])
# test_set[,c(1,3,5:65)] = scale(test_set[,c(1,3,5:65)])


#Fitting Logistic Regression to the Training Set
classifier = glm(formula = Affected ~ .,
                 family = "binomial",
                 data = training_set,
                 maxit = 50)

summary(classifier)
plogis(predict(classifier, test_set, type = "response"))
predict(classifier, test_set)

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



