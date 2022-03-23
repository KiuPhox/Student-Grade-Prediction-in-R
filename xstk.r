library("janitor")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("GGally")

# Import CSV
openExcel <- file.choose()
data<- read.csv(openExcel)

# Data cleaning
new_DF <- data %>% select(G1, G2, G3, studytime, failures, absences, paid, sex) 

ind <- which(is.na(new_DF), arr.ind = TRUE) # Get row index number
new_DF[ind] <- rowMeans(new_DF %>% select(G1, G2), na.rm = TRUE)[ind[, 1]] # Replace with mean of G1 and G2
ggpairs(new_DF %>% select(c(G1,G2,G3)))

plot(as.factor(new_DF$G3),col = "lightBlue",xlab="Final Score (G3)",ylab="Quantity")

new_DF <- new_DF %>% filter(G3 != 0)

# Data vizualization
new_DF <- new_DF %>% mutate_at(vars(,-G3,-absences),.funs=funs(factor)) %>% select(-c(G1,G2))

## Study time
ggplot(data = new_DF, aes(x = studytime, y = G3, fill = studytime)) + geom_boxplot(show.legend = F) + labs(x = "Study Time",y= "Final Score (G3)")

## Failure
ggplot(data = new_DF, aes(x = failures,y=G3,fill=failures)) + geom_boxplot(show.legend = F) + labs(x="Failures",y="Final Score (G3)")

## Paid
ggplot(data = new_DF, aes(x = paid, y=G3,fill=paid)) + geom_boxplot(show.legend = F) + labs(x="Failures",y="Final Score (G3)")

# Prediction
intrain <- sample(nrow(new_DF),nrow(new_DF)*.8)
grade_train <- grade[intrain,]
grade_test <- grade[-intrain,]
