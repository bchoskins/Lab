# Logistic Regression (All Tests, Males)

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

# takes our useful predictor variables for Males only
cleanup_dataset <- subset(dataset, dataset$gender == 'M', select = c(4,7,8))

# Encoding the affected ids as factor
cleanup_dataset$Affected = factor(cleanup_dataset$Affected, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(cleanup_dataset$Affected, SplitRatio = 0.75)
training_set = subset(cleanup_dataset, split == TRUE)
test_set = subset(cleanup_dataset, split == FALSE)

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Affected ~ value2,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results 
#test_set[-3] is index of column to predict
prob_pred = predict(classifier, type = 'response', newdata = test_set[-2])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
#predicting correct vs. incorrect 
# Single column must mean to many values to be accurate
cm = table(test_set[, 2], y_pred)

# Visualising the Training set results
# Issue: needs more variables to predict y value and plot
# Currently can predict binary but cannot plot
# install.packages("ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 3]) - 1, max(set[, 3]) + 1, by = 0.01)
grid_set = expand.grid(X1)
colnames(grid_set) = c('Values')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -2],
     main = 'Logistic Regression Males: All Tests (Training set)',
     xlab = 'Values',
     xlim = range(X1))
contour(X1, matrix(as.numeric(y_grid), length(X1)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 2] == 1, 'green4', 'red3'))
       

