setwd("D:/DATA/output3")

file_list <- list.files()
library(tools)
for(file in file_list){
  file_name <- file_path_sans_ext(file)
  file.rename(file, sprintf("%s_HP.csv",file_name))
}

