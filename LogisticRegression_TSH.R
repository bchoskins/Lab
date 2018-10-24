# Logistic Regression(TSH test)

# Importing the dataset
dataset = read.delim2('INMSP2004to2017Data.txt', header = TRUE, sep = "|", dec = ",", stringsAsFactor = FALSE)
# Useful variables
dataset = dataset[c(1,2,3,5,6)]
# Binary variable 
dataset$Affected <- 0
#Affect ids for comparison
dataset1 = read.delim2('affected_ids.txt', header = FALSE)

###rows for affected, merging binary data 
for (i in dataset1$V1) {
  z = which(dataset[,1] == i)
  for (y in z) {
    dataset[y,6] = 1
  }
}


# This isolates the 'CAH' test to check for individual test significance
working_data <- subset(dataset, dataset$test_name == 'TSH', select = c(1,2,3,4,5,6))

##Bingo got rid of <, >
#<5.0 and <11.0 only
working_data$value2 <- gsub("[\\><N]", "", working_data$value)

# switches values from factor to numeric values for easier 
# mathematical manipulation
working_data$value2 <- as.numeric(working_data$value2)

# Deals with missing values
working_data$value2 = ifelse(is.na(working_data$value2),
                             ave(working_data$value2, FUN = function(x) 
                               mean(x, na.rm = TRUE)), working_data$value2)

# data w/out old value column and actual test name column(since all TSH)
new_data <- subset(working_data, select = c(1,2,3,6,7))

# Encoding the affected ids as factor since our binary dependent variable
#new_data$Affected = factor(new_data$Affected, levels = c(0, 1))

#Tests without birth year (strictly test value and binary variable)
#working_data2 <- subset(working_data, select = c(4,5))

# Splitting the dataset into the Training set and Test set
# library(caTools)
# set.seed(123)
# split = sample.split(working_data$Affected, SplitRatio = 0.75)
# training_set = subset(working_data, split == TRUE)
# test_set = subset(working_data, split == FALSE)

#Feature scaling for Birth year and value (only if we use birth year)
# training_set[, c(1,5)] = scale(training_set[, c(1,5)])
# test_set[, c(1,5)] = scale(test_set[, c(1,5)])


sample_data <- new_data[sample(1:nrow(new_data), 50000,
                               replace=FALSE),]

# Fitting Logistic Regression to the Sampel set
# lab_no and lab_no:birth_year do not fit algorithm 
classifier = glm(formula = Affected ~ value2 + gender + birth_year,
                 family = binomial(),
                 data = sample_data)

summary(classifier)
#not a significant test

plot(sample_data$value2, sample_data$Affected, pch = 16, xlab = "TSH Test Value", ylab = "Affected")