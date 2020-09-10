getwd()
setwd("C:/ALY6050")
mtest = read.csv("mowertestresults.csv", header = TRUE )

s1 = sum(mtest$Sample1 == "Fail", na.rm =TRUE)
s2 = sum(mtest$Sample2 == "Fail", na.rm =TRUE)
s3 = sum(mtest$Sample3 == "Fail", na.rm =TRUE)
s4 = sum(mtest$Sample4 == "Fail", na.rm =TRUE)
s5 = sum(mtest$Sample5 == "Fail", na.rm =TRUE)
s6 = sum(mtest$Sample6 == "Fail", na.rm =TRUE)
s7 = sum(mtest$Sample7 == "Fail", na.rm =TRUE)
s8 = sum(mtest$Sample8 == "Fail", na.rm =TRUE)
s9 = sum(mtest$Sample9 == "Fail", na.rm =TRUE)
s10 = sum(mtest$Sample10 == "Fail", na.rm =TRUE)
s11 = sum(mtest$Sample11 == "Fail", na.rm =TRUE)
s12 = sum(mtest$Sample12 == "Fail", na.rm =TRUE)
s13 = sum(mtest$Sample13 == "Fail", na.rm =TRUE)
s14 = sum(mtest$Sample14 == "Fail", na.rm =TRUE)
s15 = sum(mtest$Sample15 == "Fail", na.rm =TRUE)
s16 = sum(mtest$Sample16 == "Fail", na.rm =TRUE)
s17 = sum(mtest$Sample17 == "Fail", na.rm =TRUE)
s18 = sum(mtest$Sample18 == "Fail", na.rm =TRUE)
s19 = sum(mtest$Sample19 == "Fail", na.rm =TRUE)
s20 = sum(mtest$Sample20 == "Fail", na.rm =TRUE)
s21 = sum(mtest$Sample21 == "Fail", na.rm =TRUE)
s22 = sum(mtest$Sample22 == "Fail", na.rm =TRUE)
s23 = sum(mtest$Sample23 == "Fail", na.rm =TRUE)
s24 = sum(mtest$Sample24 == "Fail", na.rm =TRUE)
s25 = sum(mtest$Sample25 == "Fail", na.rm =TRUE)
s26 = sum(mtest$Sample26 == "Fail", na.rm =TRUE)
s27 = sum(mtest$Sample27 == "Fail", na.rm =TRUE)
s28 = sum(mtest$Sample28 == "Fail", na.rm =TRUE)
s29 = sum(mtest$Sample29 == "Fail", na.rm =TRUE)
s30 = sum(mtest$Sample30 == "Fail", na.rm =TRUE)

totalfail1 <- sum(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20)
totalfail2 <- sum(s21,s22,s23,s24,s25,s26,s27,s28,s29,s30)
totalfail <- totalfail1 + totalfail2
totalfail
pfail <- (totalfail/3000);pfail

 
mtest_1  <- mtest[1:100,2:31]  # delete first column (Observation)
sum(mtest_1 == "Fail") # then sum all the data which element is "Fail"
pfail <- (totalfail/3000);pfail

bp <- dbinom(1, 100, 0.18)#question 3
x <- seq(1:20);x

pd <- cbind(x,bp )
pd

format(pd, nsmall = 5)

blw =read.csv("bladeWeights(2).csv", header = TRUE)
View(blw)
avgweight <- mean(blw$Weight)
sdweight <- sd(blw$Weight)


avgweight
sdweight

zscore <- (x-mean)/sd;zscore
x = 4.89
mean = 4.99
sd = 0.1

qnorm(0.18, mean=4.99, sd=0.10, lower.tail = TRUE, log.p = FALSE)

boxplot(blw$Sample, horizontal = TRUE)
w1 <- blw$Weight
s1 <- blw$Sample
w1_norm <- rnorm(350,mean=mean(w1, na.rm=TRUE), sd=sd(w1, na.rm=TRUE))
s1_norm <- rnorm(350,mean=mean(s1, na.rm=TRUE), sd=sd(s1, na.rm=TRUE))


boxplot(w1, w1_norm, s1, s1_norm,
        main = "Multiple boxplots for comparision",
        xlab = "",
        at = c(1,2,4,5),
        names = c("weight", "norm", "sample", "norm"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

q5 <- pnorm(zscore, lower.tail = FALSE, log.p = FALSE);q5


library(psych)
library(dplyr)
















