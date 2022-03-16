library("janitor")
library("dplyr")
library("ggplot2")
library("gridExtra")

openExcel <- file.choose()
data<- read.csv(openExcel)

# Data cleaning
new_DF <- data %>% select(G1, G2, G3, studytime, failures, absences, paid, sex)
new_DF[is.na(new_DF)] = 6

# Data vizualation
G1_pl <- ggplot(new_DF, aes(x = G1)) + geom_bar(fill = "#69b3a2", color = "white") + labs(x = "Diem ky 1", y = "So diem")
G2_pl <- ggplot(new_DF, aes(x = G2)) + geom_bar(fill = "#69b3a2", color = "white") + labs(x = "Diem ky 2", y = "So diem")
G3_pl <- ggplot(new_DF, aes(x = G3)) + geom_bar(fill = "#69b3a2", color = "white") + labs(x = "Diem ky 3", y = "So diem")
grid.arrange(G1_pl, G2_pl, G3_pl, ncol = 3)                                                                                                                     