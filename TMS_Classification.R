# Working with created datafram from combined_dataTMS.R

merge = read.delim2('combined_TMS_data.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
merge = merge[c(-1)]

#switches tms_analyte concentrations from character to numeric values for easier 
# mathematical manipulation
merge[,5:78]<- sapply(merge[,5:78],as.numeric)

# Run a check on columns with NAs: set threshold to >= 25% complete affected data per column 
#First, subset affected data
library(dplyr)
affectedData <- select(filter(merge, Affected == 1), c(1:78))
# Total number of affected persons
affectedSum = sum(affectedData$Affected)
#Checks a 25% threshold based on total number of NAs in each column
#new_data <- merge[, colSums(is.na(affectedData)) <= affectedSum*0.75]
#Checks a 75% threshold based on total number of NAs in each column
new_data <- merge[, colSums(is.na(affectedData)) <= affectedSum*0.25]


#assign new index value based on lab_no + birth_year
#new_data$new_index <- sequence(rle(new_data$birth_year)$lengths)

#install.packages('data.table')
library(data.table)
library(dplyr)
setDT(new_data)[, diff := lab_no - lag(lab_no, 1L), by = birth_year]
new_data[1,66] = 0


# Attempt to run MICE method of filling missing values
#First, remove categorical value
new_data2 <- new_data[-c(2)]
# check missingness from data
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(new_data2, 2, pMiss)
#look for missing data pattern
library(mice)
md.pattern(new_data2)



#seed missing values
library(missForest)
data.mis <- prodNA(new_data, noNA = 0.1)
#remove non-numerics
data.mis <- subset(data.mis, select = -c(1:4))
#
library(mice)
imputed_data <- mice(data.mis, m=5, maxit=50, method = 'pmm', seed = 500)

# Deals with missing values 
# replacing NAs with mean of column values
# *** maybe look into different way of doing this so
# when we go to predict
# for (i in 5:ncol(merge)) {
#   #merge[is.na(merge[,i]), i] <- mean(merge[,i], na.rm = TRUE)
#   merge[,i]= ifelse(is.na(merge[,i]),
#                     ave(merge[,i], FUN = function(x) 
#                       mean(x, na.rm = TRUE)), merge[,i])
# }

# Encoding the affected ids as factor
merge$Affected = factor(merge$Affected, levels = c(0, 1))

# time to take sample of 1000 that includes all Affected ("1") ids
# this approach will be to take a small sample of only the
# affected ids rows and columns (69), then take separate random subset of
# 931 from merge that have no affecteds. Lastly, combine the two small sample 
# datasets to get our working dataset of 1000 observations. 

affected_sample <- merge[merge$Affected == 1, ]

#all non_affected
non_affected <- merge[merge$Affected != 1, ]

# within 2004-2010 birth year range (only age range we have affected ids for)
year_range_data <- non_affected[non_affected$birth_year <= 2010,]

# sample from our preferred range
non_affected_sample <- year_range_data[sample(1:nrow(year_range_data), 931, replace = FALSE),]

# combined both sample datasets
library(dplyr)
working_data <- bind_rows(affected_sample, non_affected_sample)

# Fitting Logistic Regression to the Sample set 
classifier = glm(formula = Affected ~ lab_no:birth_year + .,
                 family = binomial(),
                 data = working_data)

# summary of fitted model
summary = summary(classifier)

# better for working with summary coefficients
# puts summary output into a table
# install.packages('broom')
library(broom)
td <- tidy(classifier)

# plot of p-values vs. coefficient (colored based on p-values)
# install.packages('ggplot2')
library(ggplot2)
ggplot(td, aes(term, p.value, fill = (p.value < 0.05))) + geom_histogram(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# run a 10-fold cross validation on working_data
#install.packages("caret")
#install.packages("klaR")
library(caret)
#library(klaR)

#took our lab_num, gender, and birth_year in case it was affected predctions
# because of continual errors 
num_dat <- working_data[,c(4:78)]
#working_data <- as.numeric(working_data$gender)

#figure out errors

Train <- createDataPartition(num_dat$Affected, p=0.6, list=FALSE)
training <- num_dat[ Train, ]
testing <- num_dat[ -Train, ]

mod_fit <- train(Affected ~ .,  data=training, method="glm", family="binomial")

predict(mod_fit, newdata=testing)
predict(mod_fit, newdata=testing, type="prob")

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit1 <- train(Affected ~ .,  data=working_data, method="glm", family="binomial",
                  trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit1, newdata=testing)
confusionMatrix(data=pred, testing$Class)

#### Time to go into sets based on gender ##### 
#Note: working_data already has our males + females
# boxplot(Affected ~ gender, data = working_data)
# 
# males <- working_data[working_data$gender == "M",]
# 
# females <- working_data[working_data$gender == "F",]




#create boxplots based on gender
g <- ggplot(td, aes(genderF, p.value)) + geom_boxplot()
