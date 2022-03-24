library("janitor")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("GGally")
library("performance")

# Đọc dữ liệu
openExcel <- file.choose()
data<- read.csv(openExcel)

# Làm sạch dữ liệu
new_DF <- data %>% select(G1, G2, G3, studytime, failures, absences, paid, sex) 

ind <- which(is.na(new_DF), arr.ind = TRUE) 
new_DF[ind] <- rowMeans(new_DF %>% select(G1, G2), na.rm = TRUE)[ind[, 1]] # Thay dữ liệu NA với trung bình cộng của G1 và G3

ggpairs(new_DF %>% select(c(G1,G2,G3)))

plot(as.factor(new_DF$G3),col = "lightBlue",xlab = "Điểm cuối kỳ (G3)",ylab = "Số lượng") 

## Vì điểm 0 nhiều một cách bất thường nên ta xoá các hàng chứa điểm 0 để có mô hình hồi quy hợp lý
new_DF <- new_DF %>% filter(G3 != 0) 

# Làm rõ dữ liệu
new_DF <- new_DF %>% mutate_at(vars(,-G3,-absences),.funs=funs(factor)) %>% select(-c(G1,G2))

## Study time
ggplot(data = new_DF, aes(x = studytime, y = G3, fill = studytime)) + geom_boxplot(show.legend = F) + labs(x = "Study Time",y= "Điểm cuối kỳ (G3)")

## Failure
ggplot(data = new_DF, aes(x = failures,y=G3,fill=failures)) + geom_boxplot(show.legend = F) + labs(x="Failures",y="Điểm cuối kỳ (G3)")

## Paid
ggplot(data = new_DF, aes(x = paid, y=G3,fill=paid)) + geom_boxplot(show.legend = F) + labs(x="Paid",y="Điểm cuối kỳ (G3)")

## Sex
ggplot(data = new_DF, aes(x = sex, y=G3,fill=sex)) + geom_boxplot(show.legend = F) + labs(x="Sex",y="Điểm cuối kỳ (G3)")

# Xây dựng các mô hình hồi quy tuyến tính
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
