


setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\all_data")

mydata <- read.csv("output.txt", header = FALSE)
head(mydata)
names(mydata) <- c("model","test","t")
new_df <- subset(mydata, mydata$test==mydata$model)
head(new_df)

out <- reshape(new_df, direction = "wide", idvar = "model", timevar = "test")

write.csv(new_df, file="data_diagonal.csv")
