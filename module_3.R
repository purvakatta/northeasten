crime_report <- read.csv("Cases_registered_India.csv", header = TRUE)
dim(crime_report)
class(crime_report)

View(crime_report)
summary(crime_report)

sapply(crime_report, class)


library(tidyr)
library(lattice)
library(ggplot2)
library(Matrix)
library(glmnet)

#########################################################################################################
shuffle_index <- sample(1:nrow(mydata))
head(mydata)

mydata <- mydata[shuffle_index,]
head(mydata)

set.seed(123)
index = sample(1:nrow(mydata))%>% 
  createDataPartition(p = 0.7, list = FALSE)            #####Data partition

train.data  <- mydata[index, ]
test.data <- crime_report[-index, ]

dim(train.data)
dim(test.data)

########################################################################################################

x <- model.matrix(crime_report$Pending.Investigation.2012 ~ crime_report$Cases.Regd..During.2013 +
                    crime_report$Total.Cases.For.Investigation + 
                    crime_report$Cases.Investigated + 
                    crime_report$Investigation.Dropped +
                    crime_report$False.Cases + crime_report$Cases.Charge.Sheeted + 
                    crime_report$Cases.Sent.Up.For.Trial, 
                       data = train.data)
x=x[,-1]


fit=glmnet(x, y=crime_report$Pending.Investigation.2012)   ### Fig. 1
plot(fit, label = TRUE)
print(fit)


####################################################################################################

set.seed(123)
cr_lasso <- cv.glmnet(x, y=crime_report$Pending.Investigation.2012, alpha = 1, lambda = NULL)
cr_lasso
model <- glmnet(x, y=crime_report$Pending.Investigation.2012, alpha = 1,
                lambda = cr_lasso$lambda.min)

coef(model)
plot(cr_lasso)   #### Fig.2

x_test <- model.matrix(Pending.Investigation.2012 ~ Cases.Regd..During.2013 +
                         Total.Cases.For.Investigation + 
                         Cases.Investigated + 
                         Investigation.Dropped +
                         False.Cases + Cases.Charge.Sheeted + 
                         Cases.Sent.Up.For.Trial, test.data)[,-1]
prob <- model %>% predict(newx = x_test)
predict_classes <- ifelse(prob > 0.5, "pos", "neg")

observe_classes <- test.data$Pending.Investigation.2012
mean(predict_classes == observe_classes)
 

#########################################################################################################


lasso_model <- glmnet(x, y=crime_report$Pending.Investigation.2012, alpha = 1,
                      lambda = cr_lasso$lambda.1se)

x_test <- model.matrix(Pending.Investigation.2012 ~ Cases.Regd..During.2013 +
                         Total.Cases.For.Investigation + 
                         Cases.Investigated + 
                         Investigation.Dropped +
                         False.Cases + Cases.Charge.Sheeted + 
                         Cases.Sent.Up.For.Trial, test.data)[,-1]

prob_new <- lasso_model %>% predict(newx = x_test)
predict_classes <- ifelse(prob > 0.5, "pos", "neg")

observe_classes <- test.data$Pending.Investigation.2012
mean(predict_classes == observe_classes)


#########################################################################################################

library(lars)
lasso <- lars(x=x, y=crime_report$Pending.Investigation.2012 ,trace = TRUE)
plot(lasso)             ### Fig. 3
lasso

objects(grep("lars",search()))

coef.lars(lasso)

summary.lars(lasso)

coef(lasso,s=c(0.25,0.50,0.75,1.0),
     mode="fraction")

cv.lars(x=x,y=crime_report$Pending.Investigation.2012,K=10)    #### Fig. 4

MSElasso25=dim(10)
MSElasso50=dim(10)
MSElasso75=dim(10)
MSElasso100=dim(10)
set.seed(1)
for(i in 1:10){
  train <- sample(1:nrow(crime_report),30)
  lasso <- lars(x=x[train,],y=crime_report$Pending.Investigation.2012[train])
  MSElasso25[i]=
    mean((predict(lasso,x[-train,],s=0.25,
                  mode = "fraction")$fit-
            crime_report$Pending.Investigation.2012[-train])^2)
  MSElasso50[i]=
    mean((predict(lasso,x[-train,],s=.50,
                  mode="fraction")$fit-
            crime_report$Pending.Investigation.2012[-train])^2)
  MSElasso75[i]=
    mean((predict(lasso,x[-train,],s=.75,
                  mode="fraction")$fit-
            crime_report$Pending.Investigation.2012[-train])^2)
  MSElasso100[i]=
    mean((predict(lasso,x[-train,],s=1.00,
                  mode="fraction")$fit-
            crime_report$Pending.Investigation.2012[-train])^2)
}

mean(MSElasso25)
mean(MSElasso50)
mean(MSElasso75)
mean(MSElasso100)


boxplot(MSElasso25,MSElasso50,MSElasso75,                              #### Fig. 5
        MSElasso100,
        ylab="MSE",sub="Lasso Model",
        xlab="s=0.25      s=0.50       s=0.75      s=1.00(LS)")


####################################################################################################
######################## RIDGE REGRESSION ########################################################

library(caret)




crime_report <- na.omit(crime_report)

set.seed(100)
index = sample(1:nrow(crime_report))%>% 
  createDataPartition(p = 0.7, list = FALSE)    ###Data Partition

train.data  <- crime_report[index, ]
test.data <- crime_report[-index, ]

dim(train.data)
dim(test.data)

#######################################################################################################


cols = c('Cases.Sent.Up.For.Trial', 'Cases.Pending.Investigation.At.The.end.Of.Year'
         , 'Cases.Resulted.In.Recovery.Or.Seizure', 'Cases.Pending.Trial.2012')

preproc_val <- preProcess(train.data[,cols], method = c("center", "scale"))

train.data[,cols] = predict(preproc_val, train.data[,cols])
test.data[,cols] = predict(preproc_val, test.data[,cols])

summary(train.data)

cols_reg = c('Cases.Sent.Up.For.Trial', 'Cases.Pending.Investigation.At.The.end.Of.Year'
             , 'Cases.Resulted.In.Recovery.Or.Seizure', 'Cases.Pending.Trial.2012', 'Total.Cases.For.Trial')

dummies <- dummyVars(Total.Cases.For.Trial ~ ., data=crime_report[,cols_reg])

train_dummies = predict(dummies, newdata = train.data[,cols_reg])
test_dummies = predict(dummies, newdata = test.data[,cols_reg])

print((train_dummies)) 
print((test_dummies))


#######################################################################################
                                             ####Buildind the Ridge Regression Model
rx = as.matrix(train_dummies)
y_train = train.data$Total.Cases.For.Trial

x_test = as.matrix(test_dummies)
y_test = test.data$Total.Cases.For.Trial

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(rx, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

summary(ridge_reg)

plot(ridge_reg, label = TRUE, main="Ridge Regression")                 ### Fig. 6

cv_ridge <- cv.glmnet(rx, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

############################################################################################


# Computing R^2 from true and predicted values
val_result <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = rx)
val_result(y_train, predictions_train, train.data)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
val_result(y_test, predictions_test, test.data)



boxplot(y_train, y_test,                                             ### Fig. 7
        main = "Boxplots for Comparison",
        xlab = "",
        at = c(1,2),
        names = c("Train Data", "Test Data"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)

#####################################################################################################


lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(rx, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

plot(lasso_reg)                                   ### Fig. 8

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best

