
dataset1 = read.delim2('INMSP2004to2017TMSData.txt', header = TRUE, sep = "|", dec = ",", stringsAsFactor = FALSE)
# Only want lab_no, tms_analyte, concentration
dataset1 = dataset1[c(1,2,3)]

# unique metabolites
# creates separate columns based on tms_analyte name + concentration value 
# gives 505,936 entries
# Note: this dataset only has lab_no, tms_amalytes, and respective concentration
# install.packages('tidyr')
library(tidyr)
new_dataset1 <- spread(dataset1, tms_analyte, concentration)

# read in data of lab_no, sex, and birth year based on lab_no
# extracted from test_name dataset
dataset2 = read.delim2('lab_sex_year.csv', header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
dataset2 = dataset2[c(2,3,4)]

# removes the duplicate rows from lab_no
new_dataset2 <- unique(dataset2[,])

# merges the two datasets to get gender and birth year into metabolite data
# based on similar lab_no
m <- merge(new_dataset1, new_dataset2, by = 'lab_no', all = FALSE)

# Binary variable column
m$Affected <- 0
#Affected ids for comparison
datasetAffected = read.delim2("newAffected_ids.csv", header = TRUE, sep = ",", dec = ",", stringsAsFactor = FALSE)
datasetAffected <- datasetAffected[,-c(1)]
datasetAffected <- as.data.frame(datasetAffected)
colnames(datasetAffected)[colnames(datasetAffected)=="datasetAffected"] <- "id"
# loops through rows to find affected, 
# merging binary data based on row index
for (i in datasetAffected$id) {
  z = which(m[,1] == i)
  for (y in z) {
    m[y,78] = 1
  }
}

# m now has all our desired variables
# rearrange dataframe the get gender, birth year, and affected at front
merge <- m[,c(1,76,77,78,2:75)]

# then go on to cut out the year from the lab_no
#must be a character class to manipulate as string
merge$lab_no <- as.character(merge$lab_no)
# cut off the first 4 digits of lab_no (AKA the birth year that we already have)
merge$lab_no <- substr(merge$lab_no, 5, 10)
#change back to a numeric class
merge$lab_no <- as.numeric(merge$lab_no)

# switches tms_analyte concentrations from character to numeric values for easier 
# mathematical manipulation
merge[,5:78]<- sapply(merge[,5:78],as.numeric)

# just writing dataset here to save it for future use
write.csv(merge, file = "combined_TMS_data.csv")
