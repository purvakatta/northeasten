# import packages ---------------------------------------------------------

library(ggplot2)
library(lmtest)


# Import data -------------------------------------------------------------

data <- read.csv("Defects.csv", header = T)


# Part 1 & 2 --------------------------------------------------------

defects <- c(data$X2010, data$X2011)[1:20]
months <- c(1:20)
md <- data.frame(months,defects)

reg.md <- lm(md$defects ~ md$months, data = md)
summary(reg.md)


# Data Viz ----------------------------------------------------------------

#Regression Plot
plot(md$months, md$defects, xlab = "Month", ylab = "Number of Defects", main = "Figure 1: Defects")
abline(reg.md, pch = 2, col = "red")


#Residual Plot
md.res <- resid(reg.md)
plot(md$months, md.res, ylab = "Residuals", xlab = "Months",
     main = "Figure 2: Month Residual Plot")
abline(0,0)

#Normal Probability Plot
qqnorm(md$defects)
qqline(md$defects)

#Durbin Watson Test
dwtest(reg.md)


# Part 4 ------------------------------------------------------------------


#polynomial Regression
reg.md1 <- lm(md$defects ~ md$months + I(md$months^2), data = md)
summary(reg.md1)

plot(md$months, md$defects, xlab = "Month", ylab = "Number of Defects", main = "Defects")
abline(reg.md, pch = 2, col = "red")
lines(smooth.spline(md$months, predict(reg.md1)), col = "blue", lwd =3, lty=3)

#Poly 2
reg.md2 <- lm(md$defects ~ md$months + I(md$months^2) + I(md$months^3), data = md)
summary(reg.md2)

#Residual Plot
md.res1 <- resid(reg.md1[2])
plot(md$months, md.res1, ylab = "Residuals", xlab = "Months",
     main = "Month^2 Residual Plot")
abline(0,0)

md.res2 <- resid(reg.md2[2])
plot(md$months, md.res2, ylab = "Residuals", xlab = "Months",
     main = "Month^3 Residual Plot")
abline(0,0)

#Normal Probability Plot
qqnorm(md$defects)
qqline(md$defects)


# Part 5 ------------------------------------------------------------------

defects1 <- c(data$X2010, data$X2011, data$X2012, data$X2013, data$X2014)
months1 <- c(1:60)
md1 <- data.frame(months1,defects1)

reg.md3 <- lm(md1$defects1 ~ md1$months1, data = md1)
summary(reg.md3)

plot(md1$months1, md1$defects1, xlab = "Month", ylab = "Number of Defects", main = "Figure 1: Defects")
abline(reg.md3, pch = 2, col = "red")

#polynomial
reg.md4 <- lm(md1$defects1 ~ md1$months1 + I(md1$months1^2), data = md1)
summary(reg.md3)

plot(md1$months1, md1$defects1, xlab = "Month", ylab = "Number of Defects", main = "Defects")
abline(reg.md3, pch = 2, col = "red")
lines(smooth.spline(md1$months1, predict(reg.md4)), col = "blue", lwd =3, lty=3)

# Part B


# import packages ---------------------------------------------------------

library(ggplot2)
library(ggcorrplot)
library(lmtest)

# Import data -------------------------------------------------------------

data <- read.csv("GPA.csv", header = T)

# Part 1 --------------------------------------------------------

yoe <- lm(data$YearsPLE ~ data$ï..YrsEducation, data = data)
summary(yoe)
plot(data$ï..YrsEducation, data$YearsPLE)
abline(yoe)

cg <- lm(data$YearsPLE ~ data$College.GPA, data = data)
summary(cg)
plot(data$College.GPA, data$YearsPLE)
abline(cg)

age <- lm(data$YearsPLE ~ data$Age, data = data)
summary(age)
plot(data$Age, data$YearsPLE)
abline(age)


# Part 2 ------------------------------------------------------------------

co <- round(cor(data), 2) # creating the correlation
co

p_ma <- cor_pmat(data) # design for p-values
p_ma

#Visualise the corr plot
ggcorrplot(co, method = "square",
           hc.order = TRUE,
           type ="lower", outline.col = "white",
           ggtheme = ggplot2::theme_minimal,
           lab = TRUE, insig = "blank",
           show.legend = FALSE,
           title = "Retention Data Heatmap")

#Visualise the corr plot w/p-values
ggcorrplot(co, method = "square",
           hc.order = TRUE,
           type ="lower", outline.col = "white",
           ggtheme = ggplot2::theme_minimal,
           lab = TRUE, p.mat = p_ma,insig = "blank",
           show.legend = FALSE,
           title = "Retention Data Heatmap")


# Part 3 ------------------------------------------------------------------

#MLR

lmreg.ren <- lm(data$YearsPLE ~ data$ï..YrsEducation + data$College.GPA + data$Age, data = data)
summary(lmreg.ren)

#Residual Plot

lmreg.res <- resid(lmreg.ren)
plot(data$YearsPLE, lmreg.res, ylab = "Residuals", xlab = "Yrs Education",
     main = "Figure 2: Month Residual Plot")
abline(0,0)

plot(data$College.GPA, lmreg.res, ylab = "Residuals", xlab = "College GPA",
     main = "Figure 2: Month Residual Plot")
abline(0,0)

plot(data$Age, lmreg.res, ylab = "Residuals", xlab = "Age",
     main = "Figure 2: Month Residual Plot")
abline(0,0)

#Normal Probability Plot

qqnorm(data$YearsPLE)
qqline(data$YearsPLE)

#Durbin Watson Test

dwtest(lmreg.ren)
