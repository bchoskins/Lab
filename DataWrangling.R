#Modeling newly indexed data
data = read.delim2('indexedTMS.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
data = data[,-c(1)]

#switches tms_analyte concentrations from character to numeric values for easier 
# mathematical manipulation
data[,5:68]<- sapply(data[,5:68],as.numeric)

nrow(unique(data['new_index']))
# issue, 5005357 of 505936 are unique index numbers because
# of some that share index/year but nor gender/concentrations

library(dplyr)
#grab all affected data 
affectedData <- select(filter(data, Affected == 1), c(1:68))

#grab all duplicate data
duplicate <- data[duplicated(data$new_index),]

#pull out the one duplicate that has an affected
saved <- select(filter(duplicate, Affected == 1), c(1:68))

#remove duplicates from data set
final <- dplyr::setdiff(data, duplicate)

#add the saved observation back into the dataset
dataFinal <- dplyr::union(saved, final, by = "new_index")

#reorder
dataFinal <- arrange(dataFinal, birth_year, new_index)

duplicate2 <- dataFinal[duplicated(dataFinal$new_index),]

#remove the saved affecteds duplcate
dataFinal <- dataFinal[-c(83431),]

#check for any duplicates
nrow(unique(dataFinal['new_index']))

duplicate3 <- dataFinal[duplicated(dataFinal$new_index),]

#write data so we don't have to redp every time 
write.csv(dataFinal, file = "goodData.csv")
