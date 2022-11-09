#a <- unz("US_Accidents_Dec21_updated.csv.zip", "US_Accidents_Dec21_updated.csv/US_Accidents_Dec21_updated.csv")

#data <- read.table("US_Accidents_Dec21_updated.csv", header=T, quote="\"", sep=",")

#library(readr)
library(dplyr)
#US_Accidents_Dec21_updated <- read_csv("5530.Project/US_Accidents_Dec21_updated.csv")

df<- read.csv("5530.Project/US_Accidents_Dec21_updated.csv", stringsAsFactors = FALSE)

glimpse(df)


# probably we want to drop rows where too many columns are missing
# for rows with only a small amount of data missing we should handle it
# probably we should also drop columns where not very much data is present
which(is.na(df[29]))

for(i in 1:ncol(df)) {       # for-loop over columns
  a = as.character(i)
  b = length(which(is.na(df[i])))
  if(b > 0){
    print(paste("Column: ", a, " has missing values and needs cleaning"))
  }
}


ncol(df)

summary(df)

library("GGally")
# ggpairs(df, columns = 1:ncol(df), title = "",  
#         axisLabels = "show", columnLabels = colnames(data[, columns]))


