# New DevGenes data

devData = read.delim2('DevGenesDatabases-ParticipantsWMetabol_DATA_LABELS_2019-03-15_1518.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
devData = devData[, c(1:99, 156)]
devData = devData[, -c(1,3,14,16,22,34,41,42,56,62,68,69,72,76:81,83:98)]
devData = devData[, c(1, 63, 64, 65, 2:62)]

colnames(devData)[colnames(devData)=="Sex"] <- "gender"
colnames(devData)[colnames(devData)=="Date.of.Birth"] <- "birth_year"
colnames(devData)[colnames(devData)=="X.DEP..Neuropsychiatric.Conditions...Autism"] <- "Affected"

devData$Affected <- ifelse(devData$Affected == TRUE | devData$Affected == 1, 1, 0)
devData <- select(filter(devData, Affected != ""), c(1:65))
devData$gender <- ifelse(devData$gender == "Male", "M", "F")

devData$birth_year <- substring(devData$birth_year, 1,4)

devData <- arrange(devData, birth_year, Metabolite.ID.)

devAffected <- select(filter(devData, Affected == 1), c(1:65))

affectedIDs <- as.data.frame(devAffected[,c(1)])

write.csv(affectedIDs, file = "newAffected_ids.csv")


write.csv(MyData, file = "MyData.csv")