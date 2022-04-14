library("janitor")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("GGally")
library("performance")
#1.  Import  data:  grade.csv
openExcel <- file.choose()
data<- read.csv(openExcel)
View(data)

#2.  Data  cleaning:  NA 
##a.new_DF có các biến chính
new_DF<- data[,c("G1","G2","G3","studytime","failures","absences","paid","sex")]
View(new_DF)
##b.
### kiểm tra NA trong  new_DF
apply(new_DF,2, function(new_DF) which(is.na(new_DF)))
### pp thay thế: xoá row có NA
new_DF<- na.omit(new_DF)
### Vì điểm 0 nhiều một cách bất thường nên ta xoá các hàng chứa điểm 0 để có mô hình hồi quy hợp lý
new_DF <- new_DF %>% filter(G3 != 0) 
View(new_DF)

#3.  Data  visualization
## giá trị thống kê mô tả (biến liên tục G1, G2, G3, absences)
### tính
mean<- apply(new_DF[,c(1,2,3,6)], 2, mean) 
median<- apply(new_DF[,c(1,2,3,6)], 2, median)
sd<- apply(new_DF[,c(1,2,3,6)], 2, sd) 
min<- apply(new_DF[,c(1,2,3,6)], 2, min)
max<- apply(new_DF[,c(1,2,3,6)], 2, max)
### results in tabular form
descriptiveStatistics<- list(mean=mean,median=median,sd=sd,min=min,max=max) # Creating list of vectors
descriptiveStatistics<- as.data.frame(descriptiveStatistics) # Converting list to data frame

## biến phân loại (studytime, failures, paid, sex)_lập bảng thống kê sl cho từng loại
studytime <- as.data.frame(table(new_DF$studytime, dnn = list("Self-study time per week")), responseName = "Students")
failures <- as.data.frame(table(new_DF$failures, dnn = list("Not pass the subject")), responseName = "Students")
paid <- as.data.frame(table(new_DF$paid, dnn = list("Take math classes outside of school")), responseName = "Students")
sex <- as.data.frame(table(new_DF$sex, dnn = list("sex")), responseName = "Students")
## đồ thị phân phối G123 (Histogram)
hist(new_DF[, "G1"], main = "Histogram of G1",col = "lightBlue", xlab = "Semester 1 exam score", ylab = "Quantity", labels = TRUE)
hist(new_DF[, "G2"], main = "Histogram of G2",col = "lightBlue", xlab = "Semester 2 exam score", ylab = "Quantity", labels = TRUE)
hist(new_DF[, "G3"], main = "Histogram of G3",col = "lightBlue", xlab = "Final score", ylab = "Quantity", labels = TRUE)
## phân phối G3 cho studytime, failures, paid, sex (boxplot)
boxplot(G3~studytime, main= "Boxplot of Studytime considering G3",col = "green",border = "blue",horizontal = FALSE,data=new_DF)
boxplot(G3~failures, main= "Boxplot of Failures considering G3",col = "green",border = "blue",horizontal = FALSE,data=new_DF)
boxplot(G3~paid, main= "Boxplot of Paid considering G3",col = "green",border = "blue",horizontal = FALSE,data=new_DF)
boxplot(G3~sex, main= "boxplot of Sex considering G3",col = "green",border = "blue",horizontal = FALSE,data=new_DF)
## phân phối G3 theo G1, G2, absences (pairs)
pairs(G3~G1,main= "Pairs of G1 considering G3",col = "red", pch = "8", data=new_DF)
pairs(G3~G2,main= "Pairs of G2 considering G3",col = "red", pch = "8", data=new_DF)
pairs(G3~absences,main= "Pairs of Absences considering G3",col = "red", pch = "8", data=new_DF)

# Xây dựng các mô hình hồi quy tuyến tính
new_DF <- new_DF %>% mutate_at(vars(,-G3,-absences),.funs=funs(factor)) %>% select(-c(G1,G2))
M1 <- lm(formula = G3~.,data = new_DF)
M2 <- lm(formula = G3~.-failures, data = new_DF)
M3 <- lm(formula = G3~.-studytime, data = new_DF)
M4 <- lm(formula = G3~.-paid, data = new_DF)
compare_performance(M1, M2, M3, M4) # Vì M4 có giá trị AIC thấp nên ta chọn làm mô hình hồi quy tuyến tính

plot(M4$fitted.values, M4$residuals,pch = 16,col = "black",xlab = "Fitted Values", ylab="Sai số",main = "Residual Plot")
abline(h=0,col="red")

# Dự báo điểm
new_X <- new_DF %>% select(G3)
new_X$G3_predicted <- round(predict(object = M4, newdata = new_DF), 2)

x <- cbind(Dat = sum(new_X$G3 >= 10) / nrow(new_X),Ko_Dat = sum(new_X$G3 < 10) / nrow(new_X))
y <- cbind(Dat = sum(new_X$G3_predicted >= 10) / nrow(new_X), Ko_Dat = sum(new_X$G3_predicted < 10) / nrow(new_X))
rbind(x, y)
