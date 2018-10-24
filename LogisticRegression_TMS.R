# All metabolites

# install.packages('tidyr')
# intsall.packages('dplyr')

# Importing the dataset
# new data from merge 
dataset1 = read.csv('combined_TMS_data.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)

dataset = read.delim2('INMSP2004to2017TMSData.txt', header = TRUE, sep = "|", dec = ",", stringsAsFactor = FALSE)
dataset = dataset[c(1,2,3)]



#unique metabolites
# create separate column based on tms_analyte name + concentration
# gives 505,936 entries
library(tidyr)
new_dataset <- spread(dataset, tms_analyte, concentration)

# rows from first affected id to last affected id
# check <- new_dataset[1422:275354,]

# take sample here
# 1000 samples
samples <- new_dataset[sample(1422:275354, 100000, replace=FALSE),]

# sample_data <- new_dataset[sample(1:nrow(new_dataset), 500000,
#                                replace=FALSE),]

# Binary variable column
samples$Affected <- 0
#Affected ids for comparison
dataset1 = read.delim2('affected_ids.txt', header = FALSE)

###rows for affected, merging binary data
### when running this on a smaller sample, note that all affected ids 
### may still be in the sample set when predicting 
for (i in dataset1$V1) {
  z = which(samples[,1] == i)
  for (y in z) {
    samples[y,76] = 1
  }
}

# switches values from factor to numeric values for easier 
# mathematical manipulation
sample_n_data<- sapply(samples[,],as.numeric)

# Deals with missing values 
# replacing NAs with mean of column values
for (i in 3:ncol(sample_n_data)-1) {
  print(paste(i))
  sample_n_data[,i]= ifelse(is.na(sample_n_data[,i]),
                             ave(sample_n_data[,i], FUN = function(x) 
                              mean(x, na.rm = TRUE)), sample_n_data[,i])
}

# dealing with column that has no values from sample to 
# be able to take a mean
for (i in 3:ncol(sample_n_data)-1) {
  sample_n_data[,i] = ifelse(is.na(sample_n_data[,i]), 0, sample_n_data[,i])
}

#back to data.frame from matrix to fit models
sample.df <- as.data.frame(sample_n_data)

#removed lab_no to test only metabolites
# data_min_labNo <- sample.df[-c(1)]

# Encoding the affected ids as factor
data_min_labNo$Affected = factor(data_min_labNo$Affected, levels = c(0, 1))

# Fitting Logistic Regression to the Sample set
classifier = glm(formula = Affected ~ .,
                 family = binomial(),
                 data = data_min_labNo)

# summary of fitted model
summary = summary(classifier)

# coefs = summary$coefficients
# p_vals = summary$coefficients[,4]

### all good above

# install.packages('broom')
library(broom)
td <- tidy(classifier)

library(ggplot2)
ggplot(td, aes(term, p.value)) + geom_histogram(color = "blue", fill = "white", stat = "identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
                      
