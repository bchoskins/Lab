#mlr hyperparametization practice

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


#####Playing around with mlr hyperparameterization#####


#Specify search space
library(mlr)
num_ps = makeParamSet(
  makeNumericParam("C", lower = -10, upper = 10, trafo = function(x) 10^x),
  makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 10^x)
)


#Specify optimization algorithm (note random grid search with non-discrete space)
ctrl = makeTuneControlRandom(maxit = 200L)

# evaluation method
rdesc = makeResampleDesc("CV", iters = 3L)
measure = acc

affected.task <- makeClassifTask(data = train_transformed, target = "Affected")

res = tuneParams("classif.ksvm", task = affected.task, resampling = rdesc, par.set = num_ps,
                 control = ctrl, measures = list(acc, setAggregation(acc, test.sd)), show.info = FALSE)
res
# Tune result:
#   Op. pars: C=1.63e-05; sigma=2.7e-10
# acc.test.mean=0.9746137,acc.test.sd=0.0048111

res$x
# $C
# [1] 1.632365e-05
# 
# $sigma
# [1] 2.703121e-10

res$y
# acc.test.mean   acc.test.sd 
# 0.974613687   0.004811147

#We can generate a Learner (makeLearner()) with optimal hyperparameter settings as follows:
  
lrn = setHyperPars(makeLearner("classif.ksvm"), par.vals = res$x)
lrn

m = train(lrn, affected.task)
predict(m, task = affected.task)
# Prediction: 2718 observations
# predict.type: response
# threshold: 
#   time: 0.05
# id truth response
# 1  1     1        1
# 2  2     1        1
# 3  3     1        1
# 4  4     1        1
# 5  5     1        1
# 6  6     1        1
# ... (#rows: 2718, #cols: 3)

generateHyperParsEffectData(res, trafo = TRUE)
# HyperParsEffectData:
#   Hyperparameters: C,sigma
# Measures: acc.test.mean,acc.test.sd
# Optimizer: TuneControlRandom
# Nested CV Used: FALSE
# Snapshot of data:
#   C        sigma acc.test.mean acc.test.sd iteration exec.time
# 1 1.525427e-02 1.081223e+03     0.9746137 0.004811147         1     1.000
# 2 4.266587e-03 4.222873e-07     0.9746137 0.004811147         2     0.235
# 3 6.361840e+05 1.434911e-02     0.9687270 0.003372020         3     0.601
# 4 1.005891e-04 3.843823e-04     0.9746137 0.004811147         4     0.292
# 5 3.505891e+06 1.161510e-09     0.9746137 0.004811147         5     0.285
# 6 8.850142e+03 1.206735e+04     0.9746137 0.004811147         6     1.846

rdesc2 = makeResampleDesc("Holdout", predict = "both")
res2 = tuneParams("classif.ksvm", task = affected.task, resampling = rdesc2, par.set = num_ps,
                  control = ctrl, measures = list(acc, setAggregation(acc, train.mean)), show.info = FALSE)
generateHyperParsEffectData(res2)
# HyperParsEffectData:
#   Hyperparameters: C,sigma
# Measures: acc.test.mean,acc.train.mean
# Optimizer: TuneControlRandom
# Nested CV Used: FALSE
# Snapshot of data:
#   C     sigma acc.test.mean acc.train.mean iteration exec.time
# 1 -4.211252  6.619828     0.9757174      0.9740618         1     0.123
# 2 -7.464004 -8.097836     0.9757174      0.9740618         2     0.128
# 3  3.636032  7.161010     0.9757174      1.0000000         3     0.905
# 4  3.307510 -2.632518     0.9525386      1.0000000         4     0.312
# 5 -9.599643  3.156519     0.9757174      0.9740618         5     0.108
# 6 -5.620907 -4.862705     0.9757174      0.9740618         6     0.113

res = tuneParams("classif.ksvm", task = affected.task, resampling = rdesc, par.set = num_ps,
                 control = ctrl, measures = list(acc, mmce), show.info = FALSE)
data = generateHyperParsEffectData(res)
plotHyperParsEffect(data, x = "iteration", y = "acc.test.mean",
                    plot.type = "line")

