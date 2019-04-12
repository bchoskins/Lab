
#new code so far

#time normalized data
load("/sdata/devGenes/metabolites/normalized_metabolite_data.Rdata")

#data to attach birth year and gender
full = read.delim2('/wdata/rotating_students/bhoskins/Lab_BH/lab_sex_year.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
full = full[,-c(1)]

#dataset of affected ids to attach to data
datasetAffected = read.delim2("/wdata/rotating_students/bhoskins/Lab_BH/newAffected_ids.csv", header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
datasetAffected <- datasetAffected[,-c(1)]
datasetAffected <- as.data.frame(datasetAffected)
colnames(datasetAffected)[colnames(datasetAffected)=="datasetAffected"] <- "id"

#this removes missing data of more than 30,000 observations 
#also does rough fixing of NAs
data <- as.data.frame(norm)
#gets rid of 8 metabolites
data = data[,colSums(is.na(data)) < 400000]
data = na.roughfix(data)

#adds in gender colum 
#data$gender = ""

#turns row names of id into their own column and then renames to match "full"
#still not duplicated
data$id = row.names(data)
#data <- data[,c(66, 1:65)]
colnames(data)[colnames(data)=="id"] <- "lab_no"

#remove duplicate rows from "full" before joining to data
additional <- unique(full[,])

#attachs gender and birth year to the normalized data based by lab_no
fullData <- left_join(data, additional, by = "lab_no")

fullData$Affected = 0 
fullData$Affected[match(datasetAffected$id, fullData$lab_no)] = 1

fullData$birth_year = NULL
fullData$lab_no = NULL
#fullData$gender = NULL

table(fullData[fullData$Affected == 1,'gender']) #M/F = 76/22, so we will get 760 M & 220 F

male_rows = sample(1:nrow(fullData[fullData$Affected == 0 & fullData$gender == 'M',]), 7600)
female_rows = sample(1:nrow(fullData[fullData$Affected == 0 & fullData$gender == 'F',]), 2200)

male_df = fullData[fullData$Affected == 0 & fullData$gender == 'M',][male_rows,]
female_df = fullData[fullData$Affected == 0 & fullData$gender == 'F',][female_rows,]

# object for training
x = rbind(male_df, female_df)
x = rbind(x, fullData[fullData$Affected == 1,])
x$gender = NULL
x$Affected = as.factor(x$Affected)
colnames(x) = make.names(colnames(x))

# train model to find features which are negatively important, which we will drop
model = randomForest(Affected~., x, strata=x$Affected, sampsize=c(98,98), do.trace=T, prob=T, ntree=500, importance=T)

# drop bad features
x_drop = x[,colnames(x) %in% c('Affected',row.names(model$importance[model$importance[,'MeanDecreaseAccuracy'] > 0,]))]

tuned <- tuneRF(x_drop[,-43], x_drop[,43], stepFactor=2, ntreeTry = 500)
# mtry          OOBError
# 3.OOB     3 0.000999000999001
# 6.OOB     6 0.000999000999001
# 12.OOB   12 0.000999000999001


#these guys had 49 variables with 76000 and 22000
# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(1000,98), do.trace=T, prob=T, ntree=500)
# Area under the curve: 0.564071533

# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(200,98), do.trace=T, prob=T, ntree=500)
# Area under the curve: 0.56641087

# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(200,98), do.trace=T, prob=T, ntree=1000, mtry = 8)
# Area under the curve: 0.565033111

#after decresase in gender sample
#had 41 variable swith 7600 and 2200
# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(200,98), do.trace=T, prob=T, ntree=2500, mtry = 8)
# Area under the curve: 0.576353082

# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(200,98), do.trace=T, prob=T, ntree=2500, mtry = 12)
# Area under the curve: 0.584268013


# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(300,98), do.trace=T, prob=T, ntree=5000, mtry = 12)
# Area under the curve: 0.584781341

# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(400,98), do.trace=T, prob=T, ntree=5000, mtry = 12)
# Area under the curve: 0.589397126




#new sample
# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(150,98), do.trace=T, prob=T, ntree=20000, mtry = 12)
# Area under the curve: 0.589617868

# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(125,98), do.trace=T, prob=T, ntree=2500, mtry = 12)
# Area under the curve: 0.593224177

# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(145,98), do.trace=T, prob=T, ntree=2499, mtry = 12)
# Area under the curve: 0.593709392

model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(145,98), do.trace=T, prob=T, ntree=25000, mtry = 12)
roc(x_drop$Affected, model$votes[,2])







name = paste("/wdata/rotating_students/bhoskins/Lab_BH/posterModel")
save(model, file = name)

rf.roc <- roc(training$Affected, model$votes[,2])
#Area under the curve: 0.544289796



#model 2
levels(training$Affected) <- c("first_class", "second_class")

nmin <- sum(training$Affected == "second_class")

ctrl <- trainControl(#method = "cv",
  classProbs = TRUE,
  search = "random")
#summaryFunction = twoClassSummary)

rf.fit <- train(Affected ~., data=training,
                method="rf",
                ntree=2500,
                tuneLength=10,
                trControl = ctrl,
                metric="Accuracy",
                strata=training$Affected,
                sampsize=c(98,98))

rf.roc <- roc(training$Affected, rf.fit$finalModel$votes[,2])
plot(rf.roc)
auc(rf.roc)
