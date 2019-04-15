
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

male_rows = sample(1:nrow(fullData[fullData$Affected == 0 & fullData$gender == 'M',]), 760)
female_rows = sample(1:nrow(fullData[fullData$Affected == 0 & fullData$gender == 'F',]), 220)

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

tuned <- tuneRF(x_drop[,-37], x_drop[,37], stepFactor=2, ntreeTry = 500)
print(tuned)
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
#had m=76000 and f=2200
# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(150,98), do.trace=T, prob=T, ntree=20000, mtry = 12)
# Area under the curve: 0.589617868

# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(125,98), do.trace=T, prob=T, ntree=2500, mtry = 12)
# Area under the curve: 0.593224177

# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(145,98), do.trace=T, prob=T, ntree=2499, mtry = 12)
# Area under the curve: 0.593709392


#new sample 
#had m=760 and f=220
# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(150,98), do.trace=T, prob=T, ntree=2500, mtry = 10,
#                      nodesize=2)
# Area under the curve: 0.602197001


# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(135,98), do.trace=T, prob=T, ntree=2500, mtry = 10,
#                                           nodesize=2)
# Area under the curve: 0.602785298


# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(135,98), do.trace=T, prob=T, ntree=2500, mtry = 10,
#                      nodesize=3)
# Area under the curve: 0.603795294


# model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(110,98), do.trace=T, prob=T, ntree=5000, mtry = 10,
#                      nodesize=3)
# Area under the curve: 0.606278634

#new sample
#had m=1520 and f=440

tuned <- tuneRF(x_drop[,-37], x_drop[,37], stepFactor=2, ntreeTry = 5000)

model = randomForest(Affected~., x_drop, strata=x_drop$Affected, sampsize=c(110,98), do.trace=T, prob=T, ntree=5000, mtry =10,
                     nodesize=3)

roc.rf <- roc(x_drop$Affected, model$votes[,2])
auc(roc.rf)
plot(roc.rf)

varImpPlot(model)

variableImportance = varImp(model)

name = paste("/wdata/rotating_students/bhoskins/Lab_BH/goodModel1")
save(model, file = name)

# model$confusion
# 0  1     class.error
# 0 899 81 0.0826530612245
# 1  83 15 0.8469387755102

x_drop$pred <- predict(model, x_drop)

confusionMatrix(data = x_drop$pred, reference = x_drop$Affected, positive='Affected')


dist <- list()
for ( i in  1:length(roc.rf$sensitivities)) {
  c = sqrt(((1-roc.rf$specificities[i])-0)^2 + (roc.rf$sensitivities[i]-1)^2)
  dist[[paste0("distance", i)]] <- c
}

# which(dist==Reduce(min,dist))
# distance654 
# 654 
# > roc.rf$sensitivities[654]
# [1] 0.581632653061
# > roc.rf$specificities[654]
# [1] 0.628571428571
# 


#
# control <- trainControl(method="repeatedcv", number=10, repeats = 3, search = "random")
# metric <- "Accuracy"
# mtry <- sqrt(ncol(x_drop))
# tunegrid <- expand.grid(.mtry=mtry)
# 
# model2 <- train(Affected~., data=x_drop, method="rf", metric=metric, tuneLength=15, trControl=control)
# print(model2)
# roc(x_drop$Affected, model2$finalModel$votes[,2])

#Standard: Area under the curve: 0.532099125
#Random Search: 



#shuffled AUC
shuffle <- transform(x_drop, Affected = sample(Affected) )

prop.table(table(shuffle$Affected))

shuffleModel = randomForest(Affected~., shuffle, strata=shuffle$Affected, sampsize=c(110,98), do.trace=T, prob=T, ntree=5000, mtry =10,
                     nodesize=3)

roc.shuffle1 <- roc(x_drop$Affected, shuffleModel$votes[,2])
roc.shuffle2 <- roc(x_drop$Affected, shuffleModel$votes[,2])
roc.shuffle3 <- roc(x_drop$Affected, shuffleModel$votes[,2])
roc.shuffle4 <- roc(x_drop$Affected, shuffleModel$votes[,2])
roc.shuffle5 <- roc(x_drop$Affected, shuffleModel$votes[,2])
roc.shuffle6 <- roc(x_drop$Affected, shuffleModel$votes[,2])
roc.shuffle7 <- roc(x_drop$Affected, shuffleModel$votes[,2])
roc.shuffle8 <- roc(x_drop$Affected, shuffleModel$votes[,2])
roc.shuffle9 <- roc(x_drop$Affected, shuffleModel$votes[,2])
roc.shuffle10 <- roc(x_drop$Affected, shuffleModel$votes[,2])

rocList <- list(Random_Forest_Model = roc.rf, Shuffled_Cases_Run_0 = roc.shuffle1, Shuffled_Cases_Run_1 = roc.shuffle2,
                Shuffled_Cases_Run_2 = roc.shuffle3, Shuffled_Cases_Run_3 = roc.shuffle4, Shuffled_Cases_Run_4 = roc.shuffle5,
                Shuffled_Cases_Run_5 = roc.shuffle6, Shuffled_Cases_Run_6 = roc.shuffle7, Shuffled_Cases_Run_7 = roc.shuffle8, 
                Shuffled_Cases_Run_8 = roc.shuffle9, Shuffled_Cases_Run_9 = roc.shuffle10) 

png(filename='roc_curve.png', height=8, width=8, units='in',res=300)
ggroc(rocList, aes = "colour") + ggtitle("ROC Curve") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c(rep('red',1),rep('grey',11)), name = "Model Type", breaks = c("Random_Forest_Model", "Shuffled_Cases_Run_0"), labels = c("Random Forest Model", "Shuffled Labels")) +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0)) +
  theme(text = element_text(size = 24)) +
  theme(legend.key.width = unit(3,"line"), legend.key.height = unit(1,"cm")) +
  scale_size_manual(values = c("Random Forest Model" = 2, "Shuffled Labels" = 2))
dev.off()



name = paste("/wdata/rotating_students/bhoskins/Lab_BH/shuffleModel1")
save(shuffleModel, file = name)


varImpPlot(model,n.var = 10, main = "Top 10 - Variable Importance", scale = T, sort = T) + theme(axis.text.y = element_text(size = 10))


#var_importance <- data_frame(variable=setdiff(colnames))

x_drop <- x_drop[,-c(38)]
#SVM

levels(x_drop$Affected) <- c("first_class", "second_class")

ctrl <- trainControl(method = "repeatedcv",
                         number = 5,
                         repeats = 2,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary,
                         savePredictions = TRUE)


svm.fit <- train(Affected~., data = x_drop, trControl = ctrl, method = "svmLinear", tuneLength = 10, metric = 'ROC')
svm.fit$results$ROC
# 0.50841433942

svm.fit$resample$model = 'SVM'

tunegrid <- expand.grid(.mtry=10)
#RF
rf.fit <- train(Affected ~., data=x_drop,
                method="rf",
                ntree=5000,
                tuneLength=10,
                trControl = ctrl,
                tuneGrid = tunegrid,
                nodesize = 3,
                metric="Accuracy",
                strata=x_drop$Affected,
                sampsize=c(110,98))

rf.roc <- roc(x_drop$Affected, rf.fit$finalModel$votes[,2])
auc(rf.roc)
#Area under the curve: 0.600052062

rf.fit$resample$model = 'Random Forest'

#KNN
knn_fit <- train(Affected ~., data = x_drop, method = "knn",
                 trControl=ctrl,
                 tuneLength = 10)
knn_fit$results$ROC
# [1] 0.515905612245 0.517189178303 0.525563238453 0.524985902256 0.519524704619 0.514783834586 0.505402121375 0.513087406015
# [9] 0.510147019334 0.507303302900

knn_fit$resample$model = 'KNN'

#Logistic Regression
log.fit <- train(Affected~., data = x_drop, method = "glm",
                 trControl = ctrl,
                 family = binomial())
log.fit$results$ROC
# [1] 0.535002685285

log.fit$resample$model = 'Logistic Regression'

rpart.fit <- train(Affected~., data = x_drop, method = "rpart",
                trControl = ctrl)
rpart.fit$results$ROC
rpart.fit$resample$model = 'CART'
#Naive Bayes
# library('klaR')
# nb.fit <- train(Affected~., data = x_drop, method = "nb",
#                 trControl = ctrl)
# nb.fit$results$ROC[2]
# # 0.577078410311 0.490402792696

# nb.fit$resample$model = 'nb'

#Box plot
rocData <- rbind(svm.fit$resample, knn_fit$resample, log.fit$resample, rpart.fit$resample, rf.fit$resample)

library(forcats)

png(filename='model_compare.png', height=8, width=8, units='in',res=300)
ggplot(rocData, aes(x = fct_reorder(model, ROC, fun = median, .desc =TRUE), y = ROC)) + 
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.1)) + 
  xlab("Model") +
  ylab("ROC")
dev.off()  

#Variable Importance

imp <- varImpPlot(model, n.var = 15)


library(dplyr)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  

imp <- arrange(imp, desc(MeanDecreaseGini))
imp <- imp[1:15,]

png(filename='variable_importance.png', height=8, width=8, units='in',res=300)
ggplot(imp, aes(x=reorder(varnames, MeanDecreaseGini), y=MeanDecreaseGini)) +
  geom_point(color="red") +
  geom_segment(aes(x=varnames, xend=varnames, y=0, yend=MeanDecreaseGini)) +
  ylab("Feature Importance (Gini)") +
  xlab("Metabolite") +
  coord_flip() +
  ylim(c(0,5))
dev.off()  
  


