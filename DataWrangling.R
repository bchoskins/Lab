#Modeling newly indexed data
data = read.delim2('indexedTMS.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
data = data[c(-1,-2)]

#switches tms_analyte concentrations from character to numeric values for easier 
# mathematical manipulation
data[,5:65]<- sapply(data[,5:65],as.numeric)

nrow(unique(data['new_index']))
# issue, 5005357 of 505936 are unique index numbers because
# of some that share index/year but nor gender/concentrations

library(dplyr)
affectedData <- select(filter(data, Affected == 1), c(1:65))

duplicate <- data[duplicated(data[1]),]
saved <- select(filter(duplicate, Affected == 1), c(1:65))

final <- dplyr::setdiff(data, duplicate)

dataFinal <- dplyr::union(saved, final, by = "new_index")

dataFinal <- arrange(dataFinal, birth_year, new_index)

dataFinal <- dataFinal[-c(83431),]

nrow(unique(data['new_index']))


#write data so we don't have to reindex every time (takes way too long)
write.csv(dataFinal, file = "goodData.csv")
