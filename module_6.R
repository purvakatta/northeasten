library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(Hmisc)
library(lattice)

library(tidyr)
library(dplyr)

library(lmtest)



mydata <- read.csv("climate_change.csv", header = TRUE)
dim(mydata)
class(mydata)

sapply(mydata, class)
View(mydata)

summary(mydata)


plot(mydata)      ####Fig. 1

some.data <- subset(mydata, select = c("CO2", "CH4", "N2O", "CFC.11", "CFC.12",
                                          "TSI", "Aerosols", "Temp", "MEI"))

plot(some.data)     ###Fig. 2



xx <- cor(some.data, use = 'complete.obs')
corrplot(xx, method = "number", na.label = "NA", number.cex = 0.9, tl.cex = 0.6, 
         order="hclust", col=c("black", "white"),
         bg="lightblue")                                   ### Fig. 3

ggcorrplot(xx, type = "lower", lab = TRUE)                 ### Fig. 4


####################################################################################

h <- hist(mydata$MEI , main= "Histogram-MEI", xlab="MEI",)
text(h$mids , h$counts , labels=h$counts , adj=c(0.5, -0.5))

mei <- mydata$MEI
temp <- mydata$Temp
ar <- mydata$Aerosols
ts <- mydata$TSI
par(mfrow = c(2,2))
boxplot(mei, main="MEI")
boxplot(temp, main="Temp")
boxplot(ar, main="Aerosols")
boxplot(ts, main="TSI")                                    #### Fig. 5


hist(mydata$Temp ,
     main="Histogram #1 ",
     xlab="Temperature",
     prob= TRUE,
     col= "light green", border = "dark green")
lines(density(mydata$MEI),
      lwd= 2,
      col ="dark green")                                      #### Fig. 6


hist(mydata$Temp ,
     main="Histogram #3",
     xlab="Temperature vs Aerosols",
     prob= TRUE,
     xlim = c(-0.4,1),
     col= "light blue", border = "dark blue")
lines(density(mydata$Aerosols),
      lwd= 2,
      col ="orange")                                        ### Fig. 7



ggplot(data = mydata, aes(x = Temp)) +
  geom_density() + facet_wrap(~mydata$Month)                ### Fig. 8


ggplot(data = mydata, aes(x = Temp)) +
  geom_density() + facet_wrap(~mydata$Year)                 ### Fig. 9



qplot(mydata$Year, mydata$CH4, data=mydata, geom=c("boxplot", "jitter"),
      fill=Month, main="CH4 per Month per Year",
      xlab="Year", ylab="CH4 Concentration")                                   #### Fig. 10



qplot(mydata$CFC.11, mydata$CFC.12, data=mydata, 
      fill=Month,
      alpha=I(.5),
      main="CFC Levels", xlab="CFC.11",
      ylab="CFC.12")                                 #### fig. 11



ggplot(data = mydata, aes(x=as.character(mydata$Month), y=N20)) +
  geom_boxplot(fill="steelblue") +
  labs(title="Concentration by Month", x="Month", y="N20 Concentration")     ### Fig. 12




qplot(mydata$Temp, mydata$TSI, data = mydata, color= mydata$Month,
      xlab = "Work exp", ylab = "Programming R",
      facets=mydata$Temp ~ mydata$TSI , size=I(3))


######################################################################################


counts <- table(mydata$Year)
counts
barplot(counts, main = "Unergraduate Major",
        xlab = "Types of Majors", ylab = "No of Students", )
text(counts)

prop.table(counts)

######################################################################################################
###################### HYPOTHESIS TESTINGS #########################################################
library(mcStats)
library(lmtest)



plot(mydata$Temp)
abline(reg.md)
reg.md <- lm(mydata$Temp ~ mydata$CO2 + mydata$CH4 + mydata$N2O + mydata$CFC.11 + mydata$CFC.12)

summary(reg.md)
dwtest(reg.md)

t.test(mydata$MEI, alternative = "greater")



t.test(mydata$MEI, mydata$Temp, 
             alternative = c("two.sided", "less", "greater"), paired = TRUE, 
             var.equal = FALSE)

var.test(mydata$MEI , mydata$Temp)    #### Fig. 13


  

showT.Test(mydata$MEI, mydata$Temp)



t.test(mydata$MEI, mydata$CO2)

t.test(mydata$CO2, mydata$Temp)

t.test(mydata$CO2 , mydata$CH4)


me1<- mydata$MEI[mydata$Month==1&2&3&4&5&6&7&8&9&10&11&12]
me2 <- mydata$Temp[mydata$Month==1&2&3&4&5&6&7&8&9&10&11&12]
t.test(me1,me2)


var.test(me1,me2, alternative = "two.sided")


me3 <- subset(mydata, select = c("CO2","CH4", "N2O","CFC.11", "CFC.12"))
me4 <- subset(mydata, select = c("Temp"))


showT.Test(me1, me2, paired = TRUE)                     ### Fig. 14

showT.Test(me3, me4, paired = TRUE)                     ### fig. 15

showANOVA(mydata$MEI ~ mydata$Temp)                     #### Fig. 16

showANOVA(mydata$Temp ~ mydata$CO2)


######################################################################################################


library(tidyr)
library(lattice)
library(ggplot2)
library(Matrix)
library(glmnet)
library(caret)

shuffle_index <- sample(1:nrow(mydata))
head(mydata)

mydata <- mydata[shuffle_index,]
head(mydata)

set.seed(100)
index = sample(1:nrow(mydata))%>% 
  createDataPartition(p = 0.7, list = FALSE)            #####Data partition

train.data  <- mydata[index, ]
test.data <- mydata[-index, ]

dim(train.data)
dim(test.data)

####################################################################################################
############################ LASSO REGRESSION #######################################################

x <- model.matrix(Temp ~ CO2 + CH4 + N2O + CFC.11 + CFC.12
                  + TSI + MEI + Aerosols,
                  data = train.data)
x=x[,-1]


fit=glmnet(x, y=train.data$Temp)   
plot(fit, label = TRUE)
print(fit)                                  ##### Fig. 17

set.seed(123)
cr_lasso <- cv.glmnet(x, y=train.data$Temp, alpha = 1, lambda = NULL )
cr_lasso
model <- glmnet(x, y=train.data$Temp, alpha = 1,
                lambda = cr_lasso$lambda.min)

coef(model)
plot(cr_lasso)                           #### Fig. 18


x_test <- model.matrix(Temp ~ CO2 + CH4 + N2O + CFC.11 + CFC.12
                       + TSI + MEI +Aerosols, test.data)[,-1]


prob <- model %>% predict(newx = x_test)
predict_classes <- ifelse(prob > 0.5, "pos", "neg")

observe_classes <- test.data$Temp
mean(predict_classes == observe_classes)

###################################################################################################3

lasso_model <- glmnet(x, y=train.data$Temp, alpha = 1,
                      lambda = cr_lasso$lambda.1se)

coef(lasso_model)


x_test <- model.matrix(Temp ~ CO2 + CH4 + N2O + CFC.11 + CFC.12
                       + TSI + MEI +Aerosols, test.data)[,-1]

prob_new <- lasso_model %>% predict(newx = x_test)
predict_classes_new <- ifelse(prob > 0.5, "pos", "neg")

observe_classes_new <- test.data$Temp
mean(predict_classes_new == observe_classes_new)                                  

# Best 
lambda_best <- cr_lasso$lambda.min
lambda_best


#####sequence
lambda_seq <- 10^seq(2, -2, by = -.1)

cv_output <- cv.glmnet(x, y = train.data$Temp, 
                       alpha = 1, lambda = lambda_seq)

plot(cv_output)                                 #### Fig. 19

best_lambda <- cv_output$lambda.min
best_lambda

lasso_best <- glmnet(x, y = train.data$Temp, alpha = 1, lambda = best_lambda)
lasso_best


pred_best <- predict(lasso_best, s = best_lambda, newx = x_test)
final <- cbind(test.data$Temp, pred_best)
head(final)


coef(lasso_best)

############################################################################################
################ DECISION TREE ###################################################################



part.data <- subset(mydata, select = c("CO2", "CH4", "N2O", "CFC.11", "CFC.12"))

library(rpart)
library(rpart.plot)

fit_tree <- rpart(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12, 
                  data = train.data, method = 'class', control = control)

control <- rpart.control(minsplit = 4,
                         maxdepth = 4,
                         minbucket = 4,
                         cp = 0)

summary(fit_tree)
plot(fit_tree)
text(fit_tree, pretty = 1)          
rpart.plot(fit_tree, extra = 1)   

x_test <- predict(fit_tree, test.data, type = 'class')
x_test
summary(x_test)

fit_mat <- table(test.data$Temp, x_test)
fit_mat

accuracy_test <- sum(diag(fit_mat)) / sum(fit_mat)
print(paste('Accuracy for test', accuracy_test))

###################################################################################################
#####################   RANDOM FORESTS #############################################################

library(randomForest)
library(e1071)

set.seed(1234)

rf <- randomForest(
  Temp ~.,
  data=train.data , importance = TRUE, type="class", metric ="Accuracy"
)

rf

plot(rf, main = "Random Forests")                       ### Fig. 20

which.min(rf$mse)

sqrt(rf$mse[which.min(rf$mse)])

varImpPlot(rf, main = "Random Forests", sort = TRUE)                #### Fig. 21

summary(rf)

getTree(rf)


##################################################################################

trControl <- trainControl(method = "cv", number = 10, search ="grid")

set.seed(1234)
# Run the model
rf_default <- train(Temp~.,
                    data = train.data,
                    method = "rf",
                    trControl = trControl)
# Print the results
print(rf_default)

plot(rf_default)                                               #### Fig. 22


rf_d <- randomForest(Temp~.,
                    data = train.data,
                    method = "rf",
                    trControl = trControl)
varImpPlot(rf_d, main = "Random Forests", sort = TRUE)            #### Fig. 23
rf_d$importance


set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(Temp~.,
                 data = train.data,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)

plot(rf_mtry)                                               #### Fig. 24

best_mtry <- rf_mtry$bestTune$mtry

rf_m <- randomForest(Temp~.,
                 data = train.data,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
rf_m$importance

varImpPlot(rf_m, main = "Random Forests", sort = TRUE)            ### Fig. 25


############################## SEARCH BEST MAXNODE
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(1234)
  rf_maxnode <- train(Temp~.,
                      data = train.data,
                      method = "rf",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)

results_mtry

summary(results_mtry)

rf_max <- randomForest(Temp~.,
                    data = train.data,
                    method = "rf",
                    tuneGrid = tuneGrid,
                    trControl = trControl,
                    importance = TRUE,
                    nodesize = 14,
                    maxnodes = maxnodes,
                    ntree = 300)
rf_max

rf_max$importance

varImpPlot(rf_max, main = "Random Forests", sort = TRUE)    #### Fig. 26

############### SEARCH BEST NTREES

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(Temp~.,
                       data = train.data,
                       method = "rf",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 24,
                       ntree = ntree,
                       )
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)


print(results_tree)

rf_mte <- randomForest(Temp~.,
                     data = train.data,
                     method = "rf",
                     tuneGrid = tuneGrid,
                     trControl = trControl,
                     importance = TRUE,
                     nodesize = 14,
                     maxnodes = 24,
                     ntree = ntree,
)
rf_mte
rf_mte$importance

varImpPlot(rf_mte, main = "Random Forests", sort = TRUE)           #### Fig. 27


######################################################################################################
################################# FINAL MODEL

fit_rf <- train(Temp~.,
                train.data,
                method = "rf",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 1000,
                maxnodes = 15,
                type = "class")

print(fit_rf)

f_rf <- randomForest(Temp~.,
                train.data,
                method = "rf",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 1000,
                maxnodes = 15,
                type = "class")



prediction <-predict(fit_rf, test.data)
varImpPlot(f_rf, main = "Optimal Decision Tree", sort = TRUE)   #### fig. 28


# names of features
features <- setdiff(names(train.data), "Temperature")

set.seed(123)

rf2 <- tuneRF(
  x          = train.data[features],                                #### Fig. 29
  y          = train.data$Temp,
  ntreeTry   = 1000,
  mtryStart  = 2,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

rf2

##############################################################################################
######################## TIME SERIES ANALYSIS #################################################

library(dplyr)

library(forecast)
library(tseries)


ts1 <- ts(mydata$Temp,start = c(1983,5) , frequency = 12 )
plot(ts1, main = "Time Series Plot 1", ylab = "Temperature")     ### Fig. 30

fs1 <- forecast(ts1, h=100)
autoplot(fs1)                                                   #### fig. 31

plot(fs1)



plot(ts1, main="Time Series Plot 1", ylab = "Temperature")
ls1 <- decompose(ts1)
plot(ls1)                                                          #### Fig. 32

ls <- ts1 - ls1$seasonal
ls
autoplot(ls)                                                      ##### Fig. 33

monthplot(ts1)


seqplot.ts(ts1, ts2, colx = "blue", coly = "red", typex = "l" ,
           typey = "l" , ylab = "Temperature vs MEI", main = "Time Series Plot 2")     ### Fig. 34


ts2 <- ts(mydata$MEI, start = c(1983,5) , frequency = 12)
fs2 <- forecast(ts2, h=100)
autoplot(fs2)                                                       #### Fig. 35



ts3 <- ts(mydata$Aerosols , start = c(1983,5) , frequency = 12)
seqplot.ts(ts1, ts3, colx = "blue", coly = "red", typex = "l" ,
           typey = "l" , ylab = "Temperature vs Aerosol", main = "Time Series Plot 3")   #### Fig. 36

############################################################# UNION

ts4 <- ts.union(ts2 , ts3)
fs4 <- forecast(ts4)
autoplot(fs4)                                                     #### fig. 37

Acf(ts4, plot = FALSE)
Pacf(ts4, plot = FALSE)

forecast::ggAcf(ts4)                                              ### Fig. 38
forecast::ggPacf(ts4)                                             ### fig. 39

#####################################################################

ts5 <- ts.union(ts1,ts4)
fs5 <- forecast(ts5)
autoplot(fs5)                                                    #####  Fig. 40



######################################### ARIMA MODEL 

arm <- auto.arima(mydata$Temp)
summary(arm)

par(mfrow=c(2,1))				
Acf(arm$residuals)
Acf(arm$fitted)



reg <- Arima(ts1,order=c(1,1,0),seasonal=c(1,0,0))		
reg
fc <- forecast(ts1,model=reg)		
fc

autoplot(fc)                                                       #### Fig. 41
		


forecast::gglagplot(ts1)                                            ##### Fig. 42

forecast::ggAcf(ts5)
forecast::ggPacf(ts5)



par(mfrow=c(2,1))				
Acf(arm$residuals)
Acf(arm$fitted)

############################################################################################

df <- mydata %>%
  select(Year, Temp, MEI) %>%
  gather(key = "variable", value = "value", -Year)
head(df)
                                                                     #### Fig. 43
# Multiple line plot
ggplot(df, aes(x = Year, y = value )) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()



p <- ggplot(data = mydata, aes(x = Year, y = Aerosols)) + 
  geom_line(color = "#00AFBB", size = 1)

p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"                                                    #### fig. 44
)


#######################################################################################
##############heat map

cc <- mydata[, c("CO2", "CH4",  "N2O", "CFC.11", "CFC.12")]
cc <- as.matrix(cc)

autoplot(scale(cc))                                      #### fig. 45


#####################linear model


m <- lm(Temp ~., data = mydata)
autoplot(m, which = 1:6, label.size = 3, data = mydata,                 ### fig. 46
         colour = 'Year')

#########################################################################################
############### PRINCIPAL COMPONENT ANALYSIS


pca <- prcomp(mydata, scale. = TRUE)
autoplot(pca, loadings = TRUE, loadings.label = TRUE,
         data = mydata, colour = 'Temp')                                  #### Fig. 47

###############################################################################################
#################### CLUSTERING

ht <- fortify(kmeans(mydata[-5], 4), data = mydata)
head(ht)
ggplot(ht, aes(x= cluster, fill = cluster)) + geom_bar()                        #### fig. 48


autoplot(kmeans(mydata, 10), data = mydata,
         label = TRUE, label.size = 3, frame = TRUE)                           ### Fig. 49


library(cluster)
autoplot(pam(mydata[-5], 3), frame = TRUE, frame.type = 'norm')                #### Fig.50








library(ggfortify)
library(changepoint)
library(strucchange)
library(ggpmisc)






# ggplot(df, aes(x = Year, y = value)) + 
#   geom_area(aes(color = variable, fill = variable), 
#             alpha = 0.5, position = position_dodge(0.8)) +
#   scale_color_manual(values = c("#00AFBB", "#E7B800")) +
#   scale_fill_manual(values = c("#00AFBB", "#E7B800"))
# 





