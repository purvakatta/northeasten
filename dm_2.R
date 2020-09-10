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

####################################################################################################

########### finding the corelation

library(GGally)


####plotting correlation between price, bedroom, bethroon, sqft_living, sqft_lot
plot1<-ggpairs(data=kc_data, columns=2:6,
               mapping = aes(color = "dark green"),
               axisLabels="show")
plot1


###### plotting corrlation btw price, floors, waterfront, view, condition, grade, 
plot2<-ggpairs(data=kc_data, columns=c(2,7:11),
               mapping = aes(color = "dark green"),
               axisLabels="show")
plot2

#####plotting correlation between price, yr_built, yr_renovated, lat and long

plot3=ggpairs(data=kc_data, columns=c(2,14,15,17,18),
              mapping = aes(color = "dark green"),
              axisLabels="show")
plot3

################################# visualization of correlation

### after correlation visualizations, we get to know that our most important variables are. sqft_living, bathrooms,
#### grade, view and lat


####grouping important varibales to build a model. 


############building linear regression model
library(hydroGOF)



##### splitting the dataset

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


################ Building the linear model on the data

model_train <- lm(log(price) ~ . , data = train.data)
  
summary(model_train)  


pred.model <- predict(model_train, newdata = test.data)

actual_pred <- data.frame(cbind(actuals=test.data$price, predicteds=pred.model))
s

cor.model<- cor(actual_pred)
head(cor.model)


prob <- model_train %>% predict(newx = test.data)
predict_classes <- ifelse(prob > 0.5, "pos", "neg")

observe_classes <- test.data$price
mean(predict_classes == observe_classes)


plot(model_train)

###################################################

library(ggplot2)
library(qqplotr)

ggNormQQPlot <- kc_data %>%
                                             ###plot on normal data
  ggplot(mapping = aes(sample = price)) +
  # add the stat_qq_band
  qqplotr::stat_qq_band(
    bandType = "pointwise",
    mapping = aes(fill = "Normal"), alpha = 0.5,
    show.legend = FALSE
  ) +
  # add the lines
  qqplotr::stat_qq_line() +
  # add the points
  qqplotr::stat_qq_point() +
  # add labs
  ggplot2::labs(
    x = "Theoretical Quantiles",
    y = "Sample Residuals",
    title = "Normal Q-Q plot for Original Data"
  )
ggNormQQPlot

#####################################

ggHistNormResid <- model_train %>%
  ggplot2::ggplot(aes(x = .resid)) +
  ggplot2::geom_histogram(aes(y = ..density..),
                          colour = "darkred",
                          fill = "firebrick",
                          alpha = 0.3,
                          bins = 30
  ) +
  ggplot2::stat_function(
    fun = dnorm,
    args = list(
      mean = mean(model_train$residuals, na.rm = TRUE),
      sd = sd(model_train$residuals, na.rm = TRUE)
    ),
    color = "darkblue",
    size = 1
  ) +
  ggplot2::labs(
    x = "Residuals",
    y = "Density",
    title = "The Residuals vs. Normal Distribution",
    subtitle = "Using Linear Model"
  )
ggHistNormResid


#############################################################################################



