heart <- read.csv("heart.csv", header = TRUE)
dim(heart)
sapply(heart, class)

View(heart)
summary(heart)

head(heart)

tail(heart)
#####################################################################################################

shuffle_index <- sample(1:nrow(heart))
head(shuffle_index)

heart <- heart[shuffle_index,]
head(heart)

####################################################################################################

library(dplyr)


heart <- select(heart, -c(slope, ca, target, restecg, exang, oldpeak, thal))
View(heart)

###################################################################################################


spec = c(train = .5, validation = .3, test = .2)

g = sample(cut(
  seq(nrow(heart)), 
  nrow(heart)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(heart, g)

sapply(res, nrow)/nrow(heart)

res$train

train.data   <- floor(res$train   * nrow(heart))
valid.data <- floor(res$validation * nrow(heart))
test.data <- floor(res$test * nrow(heart))

dim(train.data)
dim(valid.data)
dim(test.data)


prop.table(table(train.data$fbs))
prop.table(table(valid.data$fbs))
prop.table(table(test.data$fbs))

##################################################################################################

library(rpart)
library(rpart.plot)

fit <- rpart(sex~., data = train.data, method = 'class')
summary(fit)
plot(fit)
text(fit, pretty = 1)          ### Fig. 1 
rpart.plot(fit, extra = 106)   ### Fig. 2

x_test <- predict(fit, valid.data, type = 'class')
x_test
summary(x_test)

fit_mat <- table(valid.data$sex, x_test)
fit_mat

accuracy_test <- sum(diag(fit_mat)) / sum(fit_mat)
print(paste('Accuracy for test', accuracy_test))

##################################################################################################

accuracy_ftn <- function(fit) {
  x_test <- predict(fit, valid.data, type = 'class')
  fit_mat <- table(valid.data$sex, x_test)
  accuracy_test <- sum(diag(fit_mat)) / sum(fit_mat)
  accuracy_test
}


control <- rpart.control(minsplit = 4,
                         mindepth = 3,
                         minbucket = round(4/3),
                         cp = 0.01)

tune_fit <- rpart(sex~., data = train.data, method = 'class', control = control)
accuracy_ftn(tune_fit)
rpart.plot(tune_fit)      ###Fig. 3

###############################################################################################

tune_fit1 <- rpart(sex~., data = valid.data, method = 'class',control = control )
accuracy_ftn(tune_fit1)
rpart.plot(tune_fit1)                 #### Fig. 4


tune_fit2 <- rpart(sex~., data = test.data, method = 'class', )
accuracy_ftn(tune_fit2)
rpart.plot(tune_fit2)              ### Fig. 5


###################################################################################################


tree = rpart(sex~., data = train.data, method = "anova",)
prp(tree)                       ### Fig. 7
rpart.plot(tree)                ### Fig. 6
plotcp(tree)                    ### Fig. 8

new_trpred = predict(tree, newdata = valid.data)
tree.sse = sum((new_trpred - valid.data$sex)^2)
tree.sse

tree$cptable

tree1 <- rpart(sex~., data = train.data, method = 'anova' , 
               control = list(cp = 0.01, xval = 14 , minsplit = 4, mindepth = 3))

plotcp(tree1)                   ### Fig.9



