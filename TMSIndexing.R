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

#sort by birth_year then lab_no
new_data <- arrange(new_data, birth_year, lab_no)

#assign new index value based on lab_no + birth_year
#install.packages('data.table')
labNum <- new_data[c(1)]

library(data.table)
library(dplyr)
labNum$index = 0
labNum[1,2]=1

labNum <- as.data.table(labNum)

#takes a hot minute to run
for(i in 2:length(labNum$lab_no)) {
  print(i)
  labNum$index[i] <- ifelse(labNum$lab_no[i] >= labNum$lab_no[i-1], labNum$lab_no[i] - labNum$lab_no[i-1] + labNum$index[i-1], labNum$lab_no[i] + labNum$index[i-1])
}

#add new index to new_data
new_data$new_index <- labNum$index

#reorder
new_data <- new_data[c(1,67,3:66,2)]
new_data <- new_data[,-c(41)]

#write data so we don't have to reindex every time (takes way too long)
write.csv(new_data, file = "indexedTMS.csv")
