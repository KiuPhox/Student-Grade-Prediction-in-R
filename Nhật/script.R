library("janitor")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("GGally")
library("performance")

#1.  Đọc dữ liệu (Import data): grade.csv
openExcel <- file.choose()
data<- read.csv(openExcel)
View(data) # kiểm tra bản dữ liệu


#2.  Làm sạch dữ liệu (Data cleaning): NA (dữ liệu khuyết)
##a.trích ra một dữ liệu con đặt tên là new_DF chỉ bao gồm các biến chính mà ta quan tâm như đã trình bày trong phần giới thiệu dữ liệu
new_DF<- data[,c("G1","G2","G3","studytime","failures","absences","paid","sex")]
View(new_DF) # kiểm tra bản dữ liệu
##b.
### Kiểm tra các dữ liệu bị khuyết trong tập tin new_DF
apply(new_DF,2, function(new_DF) which(is.na(new_DF))) # Kiểm tra và xuất ra giá trị khuyết của các biến trong dữ liệu new_DF.
### phương pháp thay thế: xoá bỏ hàng có chứa dữ liệu khuyết NA
new_DF<- na.omit(new_DF)
### Ngoài ra , Vì điểm 0 nhiều một cách bất thường nên ta xoá các hàng chứa điểm 0 để có mô hình hồi quy hợp lý
new_DF <- new_DF %>% filter(G3 != 0) 
View(new_DF) # kiểm tra lại bản dữ liệu


#3.  Data  visualization
## tính các giá trị thống kê mô tả bao gồm: trung bình,trung vị, độ lệch chuẩn, giá trị lớn nhất và giá trị nhỏ nhất
### tính
mean<- apply(new_DF[,c(1,2,3,6)], 2, mean) 
median<- apply(new_DF[,c(1,2,3,6)], 2, median)
sd<- apply(new_DF[,c(1,2,3,6)], 2, sd) 
min<- apply(new_DF[,c(1,2,3,6)], 2, min)
max<- apply(new_DF[,c(1,2,3,6)], 2, max)
### Xuất kết quả dưới dạng bảng
descriptiveStatistics<- list(mean=mean,median=median,sd=sd,min=min,max=max) # Creating list of vectors
descriptiveStatistics<- as.data.frame(descriptiveStatistics) # Converting list to data frame
## biến phân loại (studytime, failures, paid, sex)_lập bảng thống kê số lượng cho từng loại
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
boxplot(G3~sex, main= "Boxplot of Sex considering G3",col = "green",border = "blue",horizontal = FALSE,data=new_DF)
## phân phối G3 theo G1, G2, absences (pairs)
pairs(G3~G1,main= "Pairs of G1 considering G3",col = "red", pch = "8", data=new_DF)
pairs(G3~G2,main= "Pairs of G2 considering G3",col = "red", pch = "8", data=new_DF)
pairs(G3~absences,main= "Pairs of Absences considering G3",col = "red", pch = "8", data=new_DF)

#4 Xây dựng các mô hình hồi quy tuyến tính
##2 tham số có mối tương quan chặt chẽ và trực tiếp với G3 là G1 và G2.
##Do đó, tình huống này có thể làm sai lệch mô hình, vì vậy cần loại bỏ G1 và G2.
## Dù loại bỏ biến khác thì nếu còn G1G2 thì mô hình vẫn khả năng đúng cao , ==> dư thừa 
new_DF <- new_DF %>% mutate_at(vars(,-G3,-absences),.funs=funs(factor)) %>% select(-c(G1,G2))
## Mô hình 1:  mô hình hồi quy tuyến tính bao gồm biến G3 là một biến phụ thuộc, và các biến còn lại đều là biến độc lập (đã trừ G1 G2)
model_new_DF_all <- lm(G3 ~ studytime + failures + absences + paid + sex,data = new_DF) 
summary(model_new_DF_all)
## Mô hình 2: Loại bỏ biến Paid từ M1
model_new_DF_selected02 <- lm(formula = G3~.-paid, data = new_DF) 
summary(model_new_DF_selected02)
## Mô hình 3: Loại bỏ biến Failures từ M1
model_new_DF_selected03 <- lm(formula = G3~.-failures, data = new_DF) 
summary(model_new_DF_selected03)
## Mô hình 4: Loại bỏ biến Studytimes từ M1
model_new_DF_selected04<- lm(formula = G3~.-studytime, data = new_DF) 
summary(model_new_DF_selected04)
## Vì model_new_DF_all có giá trị AIC và RMSE thấp nhất nên ta chọn mô hình 1 : model_new_DF_all làm mô hình hồi quy tuyến tính
compare_performance(model_new_DF_all, model_new_DF_selected02, model_new_DF_selected03, model_new_DF_selected04) 

## tải lại bản để giải thích sự tác động, mức độ ảnh hưởng của các biến lên điểm thi cuối kỳ
summary(model_new_DF_all)
##
##

## plot() vẽ đồ thị biểu thị sai số h quy và giá trị dự báo
plot(model_new_DF_all$fitted.values, model_new_DF_all$residuals,pch = 16,col = "black",xlab = "Giá trị dự báo", ylab="Sai số hồi quy",main = "Residual Plot")
abline(h=0,col="red")
## ý nghĩa và nhận xét 
##
##

#5 dự báo điểm
## a. Trong dữ liệu của bạn, hãy tạo thêm biến đặt tên là evaluate,....
evaluate<-prop.table(table(new_DF$G3>=10))
evaluate
## b. ......
new_X<-data.frame(new_DF[,c(2,3,4,5,6)])
new_X$pred_G3<-predict(model_new_DF_all,new_X)
evaluate1 <- prop.table(table(new_X$pred_G3>=10))
evaluate1
## c. ...
ketQua <- data.frame(cbind(evaluate,evaluate1))
rownames(ketQua)=c("Không đạt", "Đạt") 
colnames(ketQua)=c("Quan sát","Dự báo")
t(ketQua)






