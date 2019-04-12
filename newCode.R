#new code so far

#time normalized data
load("/sdata/devGenes/metabolites/normalized_metabolite_data.Rdata")

#data to attach birth year and gender
full = read.delim2('lab_sex_year.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
full = full[,-c(1)]

#dataset of affected ids to attach to data
datasetAffected = read.delim2("newAffected_ids.csv", header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
datasetAffected <- datasetAffected[,-c(1)]
datasetAffected <- as.data.frame(datasetAffected)
colnames(datasetAffected)[colnames(datasetAffected)=="datasetAffected"] <- "id"

#this removes missing data of more than 30,000 observations 
#also does rough fixing of NAs
data <- as.data.frame(norm)
#gets rid of 8 metabolites
data = data[,colSums(is.na(data)) < 300000]
data = na.roughfix(data)

#adds in gender colum 
#data$gender = ""

#turns row names of id into their own column and then renames to match "full"
#still not duplicated
data$id = row.names(data)
data <- data[,c(66, 1:65)]
colnames(data)[colnames(data)=="id"] <- "lab_no"

#remove duplicate rows from "full" before joining to data
additional <- unique(full[,])

#attachs gender and birth year to the normalized data based by lab_no
fullData <- left_join(data, additional, by = "lab_no")

#reorder
fullData <- fullData[, c(1, 67, 68, 2:66)]

###Here
#fix getting affecteds
#load in data from 
fullData$Affected = 0
for (i in datasetAffected$id) {
  z = which(fullData[,1] == i)
  for (y in z) {
    fullData[y,69] = 1
  }
}

affectedFull <- select(filter(fullData, Affected == 1), c(1:69))

colnames(datasetAffected)[colnames(datasetAffected)=="id"] <- "lab_no"

affectedFull$lab_no <- as.numeric(affectedFull$lab_no)

#good check to confirm correct ids as affected
check <- dplyr::anti_join(affectedFull, datasetAffected, by = "lab_no")

####HERE


#now that there is an affected data set and a full data set, 
#sample from the full based on M/F ratio and then add affected back in 
#and sample from the whole thing since time is normalized out

nonAffected<- select(filter(fullData, Affected == 0), c(1:69))

caseRatio <- sum(affectedFull$gender == "F")/sum(affectedFull$gender == "M")

controlRatio1 <- sum(nonAffected$gender =="F")/sum(nonAffected$gender == "M")

fixedRatio <- nonAffected[sample( which(nonAffected$gender=='F'), round(0.3029*length(which(nonAffected$gender=='F')))), ]

males <- select(filter(fullData, gender == "M"), c(1:69))

newData <- dplyr::union(males, fixedRatio)

controlRatio2 <- sum(newData$gender =="F")/sum(newData$gender == "M")

#sample certain number of males/females (add attriubute based on id)
controls = newData[sample(1:nrow(newData), 2500),]

#add back affecteds
controls$lab_no <- as.numeric(controls$lab_no)
newControls <- transform(controls, lab_no = as.numeric(lab_no))
colnames(affectedFull) <- make.names(colnames(affectedFull))

train <- dplyr::union(newControls, affectedFull)


training_set <- arrange(train, birth_year, lab_no)


# take gender, lab_no, and birth_year out back out
training <- training_set[,-c(1,2,3)]

#make affected a factor
training$Affected <- as.factor(training$Affected)

library(randomForest)

model = randomForest(Affected~., training, strata=training$Affected, sampsize=c(98,98), do.trace=T, prob=T)

name = paste("/wdata/rotating_students/bhoskins/Lab_BH/posterModel")
save(model, file = name)

rf.roc <- roc(training$Affected, model$votes[,2])
#Area under the curve: 0.544289796



#model 2
levels(training$Affected) <- c("first_class", "second_class")

nmin <- sum(training$Affected == "second_class")

ctrl <- trainControl(#method = "cv",
  classProbs = TRUE,
  search = "random",
  savePredictions = 'final')
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




#model 3 
treebag.fit <- train(Affected ~., data=training,
                method="treebag",
                trControl = ctrl,
                metric="ROC")

library(pROC)
# Select a parameter setting
selectedIndices <- treebag.fit$pred$mtry ==  2
# Plot:
treeRoc <- roc(treebag.fit$pred$obs,
         treebag.fit$pred$second_class)


#
