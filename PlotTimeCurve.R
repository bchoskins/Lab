#Plot of time vs metabolites
df = read.delim2('INMSP2004to2017TMSData.txt', header = TRUE, sep = "|", dec = ",", stringsAsFactor = FALSE)
# Only want lab_no, tms_analyte, concentration
df = df[c(1,2,3)]

library(tidyr)
new_df <- spread(df, tms_analyte, concentration)

sorted <- arrange(new_df, lab_no)

for(i in 1:nrow(sorted)) { sorted$index[i] <- i }

#samplePlot = new_df[sample(1:nrow(new_df), 1000),]

#choose a metabolite with a lot of unqiue values 
apply(new_df, 2, function(x) length(unique(x)))

#plot
library(ggplot2)
ggplot(data = sorted, aes(x=index, y=Phe)) + 
  geom_line(color="#00AFBB", size = 1, alpha = 0.1)


