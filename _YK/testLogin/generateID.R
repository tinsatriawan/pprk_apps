library(readxl)
user_list<-read_excel("Yumna/Login/user_list.xlsx")
id <- sprintf("%s%0*d", "id", 4, 1:length(user_list$nama))
result<-cbind(id,user_list)
        