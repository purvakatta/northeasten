setwd("C:/Users/purva/OneDrive/Documents")
trees
View(trees)
sapply(trees, class)
summary(trees)

summary(trees$Girth)

plot(density(trees$Volume),main= "Density Plot- Girth", col="magenta", lwd=2) # Fig.1

plot(density(trees$Girth),main= "Density Plot- Girth", col="magenta", lwd=2) #Fig.2

plot(density(trees$Height),main= "Density Plot- Girth", col="magenta", lwd=2) # Fig.3
 
hist(trees$Volume,
     main="Histogram #1",
     xlab="Volume",
     xlim=c(0,100),
     col= "chocolate", border = "brown",
     freq=FALSE, breaks = 20
) #Fig. 5


hist(trees$Height,
     main="Histogram #2",
     xlab="Height",
     xlim=c(50,100),
     col="pink",border = "red",
     freq=FALSE,) #Fig. 6


h <- hist(trees$Volume,ylim=c(0,10), main = "Histogram - Volume", xlab = "Volume")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5)) #Fig.4



# png("treeshist.png")
# layout(matrix(c(1:2), 2, 1,
#               byrow = TRUE))
ht <- hist(trees$Girth, # histogram
     col="lightblue", # column color
     border="darkblue",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Girth", 
     main = "Histogram # 3")
lines(density(trees$Girth), # density plot
      lwd = 2, # thickness of line
      col = "orange") # Fig.7
ht

library(ggplot2)


bx <- boxplot(trees$Height, main = "Trees - Height",
        xlab = "In Feet",
        ylab = "",
        col = "lightpink",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE) # Fig.8

bx

by <- boxplot(trees$Volume, 
        main = "Trees - Volume ",
        xlab = "In Feet",
        ylab = "",
        col = "light green",
        border = "dark green",
        horizontal = TRUE,
        notch = FALSE)
rug(trees$Volume, side = 1, lwd = 0.5) # Fig.9
by


hg <- trees$Height
vl <- trees$Volume
hg_norm <- rnorm(200,mean=mean(hg, na.rm=TRUE), sd=sd(hg, na.rm=TRUE))
vl_norm <- rnorm(200,mean=mean(vl, na.rm=TRUE), sd=sd(vl, na.rm=TRUE))

boxplot(hg, hg_norm, vl, vl_norm,
        main = "Boxplots for comparision",
        xlab = "in Cubic Feet",
        at = c(1,2,4,5),
        names = c("Height", "Hg norm", "Volume", "Vl norm"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE) #Fig.10


boxplot(trees$Girth ~ trees$Height,
        data=trees,
        main="Comparison of 2 Variables",
        xlab="Height",
        ylab="Girth",
        col="green",
        border="black") # Fig.11



qqnorm(trees$Girth)

par(mfrow=c(2,4)) # A 2 by 4 layout of plots
qqnorm(trees$Girth,xlab="", ylab="Length", main="Probability Plot - Girth")
for(i in 1:7)qqnorm(rnorm(20),xlab="", ylab="Simulated lengths",
                    main="Simulated") #Fig.12

summary(qqnorm(trees$Girth)) #Fig.13

dotchart(trees$Girth, main = "Dot Chart - Girth",
         xlab="Diameter - in Inches",
         col="magenta") #Fig.14


plot(Volume ~ Girth, data = trees, col ="blue", pch=19, main = "Regression Plot")
panel.smooth(Girth,Volume)
abline(lm(trees$Volume ~ trees$Girth), col="green", lwd=2) #Fig.15

model_trees <- lm(Volume ~ Girth, data = trees)
summary(model_trees)

 ##########################################################################################################


library(MASS)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
Rubber

View(Rubber)
sapply(Rubber, class)

summary(Rubber)
plot(Rubber) # Fig.16
cor(Rubber)



xx <- cor(Rubber, use = 'complete.obs')
corrplot(xx, method = "number") #Fig.17
          
ggcorrplot(xx, lab = TRUE, ggtheme = ggplot2::theme_gray()) #Fig.18
ggcorrplot(xx, type = "lower", lab = TRUE)

pairs(Rubber)

reg.out=lm(tens ~ loss + hard,data = Rubber)
options(digits = 3)
summary(reg.out)

plot(reg.out) #Fig 19-22
plot.lm(reg.out)


require(stats); require(graphics)
pairs(Rubber, panel = panel.smooth, main = "")
plot(tens ~ loss, data = trees, log = "xy")
coplot(log(tens) ~ log(loss) | hard, data = Rubber,
       panel = panel.smooth) ##Fig. 23

summary(fm1 <- lm(log(tens) ~ log(loss), data = Rubber))
summary(fm2 <- update(fm1, ~ . + log(hard), data = Rubber)) 
step(fm2)

#######################################################################################################

oddbooks <- read.csv("oddbooks.csv", header = TRUE)
oddbooks

logbk <- log(oddbooks)
logbk

reg_out <- lm(weight ~ thick * height * breadth, data = oddbooks)
summary(reg_out)

plot(reg_out) #Fig. 24-27

zz <- cor(oddbooks, use = 'complete.obs')
corrplot(zz, method = "number", 
         order="hclust", col=c("black", "white"),
         bg="lightblue") ##Fig.28

ggcorrplot(zz, lab = TRUE, ggtheme = ggplot2::theme_gray())
ggcorrplot(zz, type = "lower", lab = TRUE, method = c("square")) ##Fig.29


