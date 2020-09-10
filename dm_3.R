library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(finalfit)
library(plyr)
library(e1071)

######################### loading the Data
kc_data = read.csv("kc_house_data.csv",stringsAsFactors = FALSE)


######## to clean the bathrooms column
find <- c(1.25,1.75,2.25,2.75)
replace <- c(1,1.5,2,2.5)
found <- match(kc_data$bathrooms,find)
ifelse(is.na(found), kc_data$bathrooms , replace[found])

######################removing insignificant column
kc_data$id <- NULL

###### Correcting the Date Format
kc_data$date <- gsub('.{7}$' , '', kc_data$date)
kc_data$date <- ymd(kc_data$date)


##### to check for NA values
colSums(is.na(kc_data))


##### to check categorical and continuous variables 
ff_glimpse(kc_data)


#################################################################################################

kcd_int<- kc_data %>% select_if(is.integer)          ## grouped all the variables with data type int
str(kcd_int)

kcd_remaining<- kc_data[,!sapply(kc_data,is.integer)]
str(kcd_remaining)

################################################################

library(hydroGOF)
library(caret)
library(xgboost)

########## Data Spliiting

shuffle_index <- sample(1:nrow(kc_data))
head(kc_data)

kc_data <- kc_data[shuffle_index,]
head(kc_data)

set.seed(123)
index = sample(1:nrow(kc_data))%>% 
  createDataPartition(p = 0.7, list = FALSE)            #####Data partition

train.data  <- kc_data[index, ]
test.data <- kc_data[-index, ]

dim(train.data)
dim(test.data)

####################################################################

model_train <- lm(log(price) ~ . , data = train.data)

pred.model <- predict(model_train, newdata = test.data)


########### original model
data.frame(
  RMSE = RMSE(pred.model, log(test.data$price)),
  MSE = mse(pred.model, log(test.data$price)),
  R2 = R2(pred.model, log(test.data$price))
)



library(leaps)
library(MASS)

step_model <- stepAIC(model_train, direction = "both", trace = FALSE)

summary(step_model)

new_pred<-predict(step_model,newdata = test.data)

summary(step_model)



data.frame(
  RMSE = RMSE(new_pred, log(test.data$price)),
  MSE = mse(new_pred, log(test.data$price)),
  R2 = R2(new_pred, log(test.data$price))
)



plot(fitted(model_train), fitted(step_model), col = "dodgerblue", pch = 20,
     xlab = "Original Model ", ylab = "Model after Stepwise", cex = 1.5)
abline(a = 0, b = 1, col = "darkorange", lwd = 2)



plot(resid(model_train) ~ resid(step_model), 
     col = "dodgerblue", pch = 20,
     xlab = "Residuals, Added Predictor", 
     ylab = "Residuals, Original Model")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
abline(lm(resid(model_train) ~ resid(step_model)),
       col = "darkorange", lwd = 2)



library(car)

car::vif(step_model)

####  remove all the variables with vif >5 

model2 <- lm(log(price) ~ . -sqft_above , -sqft_living, data = train.data)
summary(model2)

pred_new <- model2 %>% predict(test.data)



data.frame(
  RMSE = RMSE(pred_new, log(test.data$price)),
  MSE = mse(pred_new, log(test.data$price)),
  R2 = R2(pred_new, log(test.data$price))
)
  
################# gradient boosting ########################### Method 1
# Fit the model on the training set

set.seed(123)
kc_model <- train(
  log(price) ~., data = train.data, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)


set.seed(123)
kc_boost <- train(
  log(price) ~., data = train.data, method = "gbm",
  trControl = trainControl("cv", number = 10), 
)

kc_boost


plot(pred(model_train), pred(kc_model), col = "dodgerblue", pch = 20,
     xlab = "Original Model ", ylab = "Model after Stepwise", cex = 1.5)
abline(a = 0, b = 1, col = "darkorange", lwd = 2)


trellis.par.set(caretTheme())
plot(kc_model, metric = "Rsquared")
plot(kc_model)


# Best tuning parameter
kc_model$bestTune


# Make predictions on the test data
pred.1 <- kc_model %>% predict(test.data)
head(pred.1)

# Compute model prediction accuracy rate
mean(pred.1 == log(test.data$price))

p<- varImp(kc_model)


ggplot(p, aes(x=varaibles, y=performance)) +
  geom_point(size = 4) +ggplot2::labs(title = "Variable Importance Plot", subtitle = "Gradient Boosting") 
  
  
  
  #geom_text(aes(label=Importance), size=3, nudge_x = 0.7, nudge_y = 0.07, check_overlap = T) 
  
  
 

# Compute the average prediction error RMSE
data.frame(
  RMSE = RMSE(pred.1, log(test.data$price)),
  MSE = mse(pred.1, log(test.data$price)),
  R2 = R2(pred.1, log(test.data$price))
)


pred.3 <- predict.train(kc_model, newdata = test.data)

mean(pred.3 == log(test.data$price))

data.frame(
  RMSE = RMSE(pred.3, log(test.data$price)),
  MSE = mse(pred.3, log(test.data$price)),
  R2 = R2(pred.3, log(test.data$price))
)


library(ggplot2)
library(lattice)

ggplot(
  data = kc_model,
  metric = kc_model$metric[1],
  plotType = "scatter",
  output = "ggplot"

)

trellis.par.set(caretTheme())
densityplot(kc_model, pch = "|", resamples="all" ,lwd = 2, 
            col = "dark green", main = "Gradient Boosting Model")


####################### method 2 #################################33

library(gbm)

kc_boost <- gbm(log(price) ~ .-date ,data = train.data, distribution = "gaussian", n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4, cv.folds = 5, n.cores = NULL)
kc_boost

summary(kc_boost) #Summary gives a table of Variable Importance and a plot of Variable Importance

#Plot of Response variable with lstat variable
plot(kc_boost,i="sqft_living") 

#price increases with the incraese in sqft_living and then fluctuates and then becomes constant. 

plot(kc_boost,i="grade") 
#as the grade increases the the price increases

#RMSE
sqrt(min(kc_boost$cv.error))

gbm.perf(kc_boost, method = "cv")

cor(kc_data$sqft_living,kc_data$price)
cor(kc_data$grade,kc_data$price)

###############################

boost_model2 <- gbm(log(price) ~ .-date ,data = train.data, distribution = "gaussian", n.trees = 10000,
                shrinkage = 0.1, interaction.depth = 4, cv.folds = 5, n.cores = NULL)


sqrt(min(boost_model2$cv.error))


min_MSE <- which.min(boost_model2$cv.error)

sqrt(boost_model2$cv.error[min_MSE])

gbm.perf(boost_model2, method = "cv")

#########testing

n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
pred2 <- predict(kc_boost,train.data,n.trees = n.trees)
dim(pred2) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error <- with(train.data , apply( (pred2 - price)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees

plot(n.trees , test.error , pch=19, col="blue",
     xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters

abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)


# Make predictions on the test data
pred.2 <- boost_model2 %>% predict(test.data)
head(pred.2)

# Compute model prediction accuracy rate
mean(pred.2 == test.data$price)

varImp(kc_model)

# Compute the average prediction error RMSE
data.frame(
  RMSE = RMSE(pred.2, test.data$price),
  R2 = R2(pred.2, test.data$price)
)

###############################################################################################
############################## PCA analysis  ###########################################################

kc_pca <- prcomp(train.data[,c(2:20)], center = TRUE,scale. = TRUE)
princomp(train.data[,c(2:20)], cor = TRUE, scores = TRUE)

summary(kc_pca)


pred.3 <- predict(kc_pca, newdata = test.data)

pred.3



data.frame(
  RMSE = RMSE(pred.3, log(test.data$price)),
  MSE = mse(pred.3, log(test.data$price)),
  R2 = R2(pred.3, log(test.data$price))
)


  

library(factoextra)

fviz_eig(kc_pca)

fviz_pca_ind(kc_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



