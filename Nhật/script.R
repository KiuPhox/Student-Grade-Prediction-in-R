library("janitor")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("GGally")
library("performance")
install.packages("GGally")
#1.  D???c d??? li???u (Import data): grade.csv
openExcel <- file.choose()
data<- read.csv(openExcel)
View(data) # ki???m tra b???n d??? li???u


#2.  Làm s???ch d??? li???u (Data cleaning): NA (d??? li???u khuy???t)
##a.trích ra m???t d??? li???u con d???t tên là new_DF ch??? bao g???m các bi???n chính mà ta quan tâm nhu dã trình bày trong ph???n gi???i thi???u d??? li???u
new_DF<- data[,c("G1","G2","G3","studytime","failures","absences","paid","sex")]
View(new_DF) # ki???m tra b???n d??? li???u
##b.
### Ki???m tra các d??? li???u b??? khuy???t trong t???p tin new_DF
apply(new_DF,2, function(new_DF) which(is.na(new_DF))) # Ki???m tra và xu???t ra giá tr??? khuy???t c???a các bi???n trong d??? li???u new_DF.
### phuong pháp thay th???: xoá b??? hàng có ch???a d??? li???u khuy???t NA
new_DF<- na.omit(new_DF)
### Ngoài ra , Vì di???m 0 nhi???u m???t cách b???t thu???ng nên ta xoá các hàng ch???a di???m 0 d??? có mô hình h???i quy h???p lý
new_DF <- new_DF %>% filter(G3 != 0) 
View(new_DF) # ki???m tra l???i b???n d??? li???u


#3.  Data  visualization
## tính các giá tr??? th???ng kê mô t??? bao g???m: trung bình,trung v???, d??? l???ch chu???n, giá tr??? l???n nh???t và giá tr??? nh??? nh???t
### tính
mean<- apply(new_DF[,c(1,2,3,6)], 2, mean) 
median<- apply(new_DF[,c(1,2,3,6)], 2, median)
sd<- apply(new_DF[,c(1,2,3,6)], 2, sd) 
min<- apply(new_DF[,c(1,2,3,6)], 2, min)
max<- apply(new_DF[,c(1,2,3,6)], 2, max)
### Xu???t k???t qu??? du???i d???ng b???ng
descriptiveStatistics<- list(mean=mean,median=median,sd=sd,min=min,max=max) # Creating list of vectors
descriptiveStatistics<- as.data.frame(descriptiveStatistics) # Converting list to data frame
## bi???n phân lo???i (studytime, failures, paid, sex)_l???p b???ng th???ng kê s??? lu???ng cho t???ng lo???i
studytime <- as.data.frame(table(new_DF$studytime, dnn = list("Self-study time per week")), responseName = "Students")
failures <- as.data.frame(table(new_DF$failures, dnn = list("Not pass the subject")), responseName = "Students")
paid <- as.data.frame(table(new_DF$paid, dnn = list("Take math classes outside of school")), responseName = "Students")
sex <- as.data.frame(table(new_DF$sex, dnn = list("sex")), responseName = "Students")
## d??? th??? phân ph???i G123 (Histogram)
hist(new_DF[, "G1"], main = "Histogram of G1",col = "lightBlue", xlab = "Semester 1 exam score", ylab = "Quantity", labels = TRUE)
hist(new_DF[, "G2"], main = "Histogram of G2",col = "lightBlue", xlab = "Semester 2 exam score", ylab = "Quantity", labels = TRUE)
hist(new_DF[, "G3"], main = "Histogram of G3",col = "lightBlue", xlab = "Final score", ylab = "Quantity", labels = TRUE)
## phân ph???i G3 cho studytime, failures, paid, sex (boxplot)
boxplot(G3~studytime, main= "Boxplot of Studytime considering G3",col = "green",border = "blue",horizontal = FALSE,data=new_DF)
boxplot(G3~failures, main= "Boxplot of Failures considering G3",col = "green",border = "blue",horizontal = FALSE,data=new_DF)
boxplot(G3~paid, main= "Boxplot of Paid considering G3",col = "green",border = "blue",horizontal = FALSE,data=new_DF)
boxplot(G3~sex, main= "Boxplot of Sex considering G3",col = "green",border = "blue",horizontal = FALSE,data=new_DF)
## phân ph???i G3 theo G1, G2, absences (pairs)
pairs(G3~G1,main= "Pairs of G1 considering G3",col = "red", pch = "8", data=new_DF)
pairs(G3~G2,main= "Pairs of G2 considering G3",col = "red", pch = "8", data=new_DF)
pairs(G3~absences,main= "Pairs of Absences considering G3",col = "red", pch = "8", data=new_DF)

#4 Xây d???ng các mô hình h???i quy tuy???n tính
##2 tham s??? có m???i tuong quan ch???t ch??? và tr???c ti???p v???i G3 là G1 và G2.
##Do dó, tình hu???ng này có th??? làm sai l???ch mô hình, vì v???y c???n lo???i b??? G1 và G2.
## Dù lo???i b??? bi???n khác thì n???u còn G1G2 thì mô hình v???n kh??? nang dúng cao , ==> du th???a 
new_DF <- new_DF %>% mutate_at(vars(,-G3,-absences),.funs=funs(factor)) %>% select(-c(G1,G2))
## Mô hình 1:  mô hình h???i quy tuy???n tính bao g???m bi???n G3 là m???t bi???n ph??? thu???c, và các bi???n còn l???i d???u là bi???n d???c l???p (dã tr??? G1 G2)
model_new_DF_all <- lm(G3 ~ studytime + failures + absences + paid + sex,data = new_DF) 
summary(model_new_DF_all)
## Mô hình 2: Lo???i b??? bi???n Paid t??? M1
model_new_DF_selected02 <- lm(formula = G3~.-paid, data = new_DF) 
summary(model_new_DF_selected02)
## Mô hình 3: Lo???i b??? bi???n Failures t??? M1
model_new_DF_selected03 <- lm(formula = G3~.-failures, data = new_DF) 
summary(model_new_DF_selected03)
## Mô hình 4: Lo???i b??? bi???n Studytimes t??? M1
model_new_DF_selected04<- lm(formula = G3~.-studytime, data = new_DF) 
summary(model_new_DF_selected04)
## Vì model_new_DF_all có giá tr??? AIC và RMSE th???p nh???t nên ta ch???n mô hình 1 : model_new_DF_all làm mô hình h???i quy tuy???n tính
compare_performance(model_new_DF_all, model_new_DF_selected02, model_new_DF_selected03, model_new_DF_selected04) 

## t???i l???i b???n d??? gi???i thích s??? tác d???ng, m???c d??? ???nh hu???ng c???a các bi???n lên di???m thi cu???i k???
summary(model_new_DF_all)
##
##

## plot() ve~ dô` thi?? biê??u thi?? sai sô?? h quy va` gia?? tri?? du?? ba??o
plot(model_new_DF_all$fitted.values, model_new_DF_all$residuals,pch = 16,col = "black",xlab = "Giá tr??? d??? báo", ylab="Sai s??? h???i quy",main = "Residual Plot")
abline(h=0,col="red")
## ý nghia và nh???n xét 
##
##

#5 d??? báo di???m
## a. Trong d??? li???u c???a b???n, hãy t???o thêm bi???n d???t tên là evaluate,....
evaluate<-prop.table(table(new_DF$G3>=10))
evaluate
## b. ......
new_X<-data.frame(new_DF[,c(2,3,4,5,6)])
new_X$pred_G3<-predict(model_new_DF_all,new_X)
evaluate1 <- prop.table(table(new_X$pred_G3>=10))
evaluate1
## c. ...
ketQua <- data.frame(cbind(evaluate,evaluate1))
rownames(ketQua)=c("Không d???t", "D???t") 
colnames(ketQua)=c("Quan sát","D??? báo")
t(ketQua)





