# Combine DevGenes Affected with previous affected

newA = read.delim2('newAffected_ids.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
oldA = read.delim2('affected_ids.txt', header = FALSE, sep = ",", dec = ",", stringsAsFactor = FALSE)

newA <- newA[, -c(1)]
newA <- as.data.frame(newA)

colnames(newA)[colnames(newA)=="newA"] <- "id"
colnames(oldA)[colnames(oldA)=="V1"] <- "id"

fullAffecteds <- union(oldA, newA)
fullAffecteds <- arrange(fullAffecteds, id)

duplicated(fullAffecteds)

write.csv(fullAffecteds, file = "fullAffecteds.csv")
