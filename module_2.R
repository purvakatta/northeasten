setwd("C:/Users/purva/OneDrive/Documents")
################### PART A -A ##################
library(MASS)

################# B ##############

View(chem)
summary(chem)
x <- chem
n <- length(x); n
m <- mean(x); m
sd <-sd(x);sd
chem
sew <- t.test(chem, alternative = "greater", mu=1)  
plot(sew)

#################### C ####################


cats
summary(cats)
male_bwt<- cats$Bwt[cats$Sex=="M"]
female_bwt <- cats$Bwt[cats$Sex=="F"]
t.test(male_bwt,female_bwt)

################### F################################

var.test(male_bwt,female_bwt, alternative = "two.sided")

################################### D ###################
shoes
summary(shoes$A)
summary(shoes$B)
t.test(shoes$A, shoes$B, paired = TRUE)

########################## E #######################

bacteria
summary(bacteria)

drugA <- subset(bacteria, subset=(bacteria$trt=="drug"))
drugB <- subset(bacteria, subset=(bacteria$trt=="drug+"))
drugC <- subset(bacteria, subset=(bacteria$trt=="placebo"))

length(drugC)
drug_subset <- subset(bacteria, subset = (bacteria$trt == 'drug+' | bacteria$trt == 'drug'))
length(drug_subset)
drug_final<- head(drug_subset,96)
prop.test(drug_final, drugC)
prop.test(drugC,drug_subset)


####################### PART B ##############################################################3
################################################################################################
#############################################################################################


whr <- read.csv("world-happiness-report-2019.csv", stringsAsFactors = FALSE)
dim(whr)
class(whr)
summary(whr)
View(whr)


############# TEST 1 ############################
t.test(whr$Social.support, whr$Corruption)

sp <- whr$Social.support
cp <- whr$Corruption
par(mfrow = c(2,2))
hist(sp, main = "Social Support")           
boxplot(sp, main="Social Support")
hist(cp, main = "Corruption")
boxplot(cp, main="Corruption")



################# TEST 2 #########################
t.test(whr$Healthy.life.expectancy, whr$Log.of.GDP.per.capita)

hlp <- whr$Healthy.life.expectancy
gdp <- whr$Log.of.GDP.per.capita
par(mfrow = c(2,2))
hist(hlp, main = "Healthy Life Expectancy")           
hist(gdp, main = "GDP Per Capita")

zz <- density(whr$Healthy.life.expectancy, na.rm = TRUE)
plot(zz, main = "Healthy Life Expectancy", col= "red", lwd=2)

yy <- density(whr$Log.of.GDP.per.capita, na.rm = TRUE)
plot(yy, main = "GDP Per Capita", col= "red", lwd=2)

library(mcStats)
showT.Test(whr$Healthy.life.expectancy, whr$Log.of.GDP.per.capita, paired = FALSE)

##################### TEST 3 ###############################

var.test(whr$Positive.affect, whr$Negative.affect)

var.test(whr$Generosity, whr$Log.of.GDP.per.capita)

library(ggplot2)
par(mfrow = c(1,2))
bplot(whr$Positive.affect, main = "Positive Affect")           
bplot(whr$Positive.affect, main = "Negative Affect")




plot(density(whr$Ladder),main= "Density Plot", col="magenta", lwd=2, )

plot(density(whr$Social.support),main= "Density Plot", col="magenta", lwd=2,)

#################### TEST 5 ###################################

t.test(whr$Corruption, y=whr$Negative.affect, 
             alternative = c("two.sided", "less", "greater"), paired = TRUE, 
             var.equal = FALSE)

plot(whr$Corruption, col="blue", pch=19, main = "Corrution and Negative Emotion", xlab = "Negative Emotion", 
      ylab = "Corruption" )
points(whr$Negative.affect, col="green")
panel.smooth(whr$Corruption, whr$Negative.affect, col = "red")








library(ggplot2)

library(ggpubr)



ggboxplot(whr$Positive.affect, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

##############################################################################################

ggqqplot(whr$Positive.affect, ylab = "positive emotion", 
         ggtheme = theme_minimal())

ggqqplot(whr$Negative.affect, ylab = "negative emotion", 
         ggtheme = theme_minimal())



boxplot(whr$Corruption ~ whr$Log.of.GDP.per.capita, xlab = "GDP per Capita", ylab = "Corruption")




ggplot(whr, aes(whr$Healthy.life.expectancy)) + 
  geom_histogram(fill = "white", color = "grey30") +
  scale_x_log10()


p1 <- qplot(whr$Positive.affect, whr$Freedom, data = whr, colour = whr$Country..region.)
p2 <- qplot(whr$Negative.affect, data = whr) + ggtitle("")
p3 <- qplot(whr$Freedom, data = whr, geom = "dotplot")
p4 <- p1 + facet_wrap( ~ whr$Log.of.GDP.per.capita, nrow = 1) + theme(legend.position = "none") +
  ggtitle("facetted plot")



ggplot(data = whr, aes(x = whr$Generosity)) +
  geom_density(aes(color = whr$Country..region.=="Denmark"))

ggplot(data = whr, aes(x = whr$Social.support)) +
  geom_density() + facet_wrap(~whr$Country..region.=="Finland")



qplot(whr$Corruption, data=whr, geom="density", fill=whr$Corruption=="Finland",
      alpha=I(.5),
      main="P&D", xlab="level of expertise",
      ylab="Density")

qplot(whr$Corruption, whr$Log.of.GDP.per.capita, data=whr, geom=c("boxplot", "jitter"),
       main="Corruption per Capita",
      xlab="", ylab="Corruption Index")

##################################################################################################

fk <- whr$Freedom
hk <- whr$Healthy.life.expectancy
fk_norm <- rnorm(200,mean=mean(fk, na.rm=TRUE), sd=sd(fk, na.rm=TRUE))
hk_norm <- rnorm(200,mean=mean(hk, na.rm=TRUE), sd=sd(hk, na.rm=TRUE))

boxplot(fk, fk_norm, hk, hk_norm,
        main = "Boxplots for Comparison",
        xlab = "Index",
        at = c(1,2,4,5),
        names = c("Freedom", "Freedom norm", "Healthy Life Expectancy", " HLP normal"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)


#####################################################################################

cr <- whr$Corruption
fr <- whr$Freedom
par(mfrow = c(2,2))
hist(cr, main = "Corruption")           
barplot(cr, main="Corruption")
hist(fr, main = "Freedom")
barplot(fr, main="Freedom")



