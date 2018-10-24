# Multiple Logistic Regression (All Tests)

# Importing the dataset
dataset = read.delim2('INMSP2004to2017Data.txt', header = TRUE, sep = "|", dec = ",", stringsAsFactor = FALSE)
# Useful variables (hospital not needed)
dataset = dataset[c(0:3, 5:7)]
# Binary variable 
dataset$Affected <- 0
# so we can create our affected ids column
dataset1 = read.delim2('affected_ids.txt', header = FALSE)

###rows for affected, merging binary data 
for (i in dataset1$V1) {
  z = which(dataset[,1] == i)
  for (y in z) {
    dataset[y,7] = 1
  }
}

#Get rid of symbols and non-numerics
dataset$value2 <- gsub("[\\><N]", "", dataset$value)

# switches values from factor to numeric values for easier 
# mathematical manipulation (creates NAs but no worries the
# next step takes care of that)
dataset$value2 <- as.numeric(dataset$value2)

# Deals with missing values
dataset$value2 = ifelse(is.na(dataset$value2),
                        ave(dataset$value2, FUN = function(x) 
                          mean(x, na.rm = TRUE)), dataset$value2)

revised_data <- subset(dataset, select = c(0:4, 7:8))

class(revised_data)

str(revised_data)

#install.packages('WGCNA')
# library(WGCNA)
# data.t = transposeBigData(revised_data)

#taking a sample size to work with
sample_data <- revised_data[sample(1:nrow(revised_data), 50,
                          replace=FALSE),]
#data.t = t(sample_data)

#tests.t <- t(sample_data$)
