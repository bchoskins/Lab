#Testing 2

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

affectedData[,c(5:65)]<- sapply(affectedData[,5:65],as.numeric)

affectedData$gender = as.factor(affectedData$gender)

affectedData$Affected = as.factor(affectedData$Affected)


apply(group2, 2, function(x) length(unique(x)))

#Make gender numeric
group2$gender = as.numeric(group2$gender)
affectedData$gender = as.numeric(affectedData$gender)

library(mice)
imputed <- mice(group2, m=5, maxit = 5, method = "cart", seed = 500)
completedData1 <- complete(imputed)
anyNA(completedData1)

###Add in the affecteds
library(gtools) 
# need to add affected data with NAs back in to impute w/ group 2
combined <- smartbind(completedData1, affectedData)
#sort so that affected data is integrated within the control data
newImpute <- arrange(combined, birth_year, new_index)

#Impute affecteds with previously imputed controls
library(mice)
imputed2 <- mice(newImpute, m=5, maxit = 5, method = "cart", seed = 500)
completedData2 <- complete(imputed2)
anyNA(completedData2)

levels(completedData2$Affected) <- c("first_class", "second_class")

nmin <- sum(completedData2$Affected == "second_class")

ctrl <- trainControl(#method = "cv",
                     classProbs = TRUE)
                     #search = "random")
#summaryFunction = twoClassSummary)

rf.fit <- train(Affected ~., data=completedData2[,-c(1,2,3)],
                method="rf",
                ntree=2000,
                tuneLength=20,
                trControl = ctrl,
                metric="ROC",
                strata=completedData2$Affected,
                sampsize=rep(nmin,2))

rf.roc <- roc(completedData2$Affected, rf.fit$finalModel$votes[,2])
auc(rf.roc)
#Area under the curve: 0.529486217
#imputed altogether: ntree = 2000, tune = 20  Area under the curve: 0.521892174
