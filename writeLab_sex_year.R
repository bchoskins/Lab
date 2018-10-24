

dataset = read.delim2('INMSP2004to2017Data.txt', header = TRUE, sep = "|", dec = ",", stringsAsFactor = FALSE)

needed_data = dataset[c(1,2,3)]

write.csv(needed_data, file='lab_sex_year.csv')