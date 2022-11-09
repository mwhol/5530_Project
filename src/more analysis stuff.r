
df2 <- na.omit(df)
my_num_data <- df2[, sapply(df2, is.numeric)]
cor(my_num_data)



