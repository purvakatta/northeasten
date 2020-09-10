project = read.csv("crimeincidentreports.csv",stringsAsFactors = FALSE)
dim(project)
class(project)

library(dplyr)
distinct(project, project$STREET)
distinct(project, project$OFFENSE_CODE_GROUP)
n_col <- project %>% count(project$OFFENSE_CODE_GROUP, sort=TRUE, name= "n_col")
##n_col <-table(project$OFFENSE_CODE_GROUP)
View(n_col)
summary(n_col)

levels(factor(project$OFFENSE_CODE_GROUP))

tabulate(project$OFFENSE_CODE_GROUP, project$REPORTING_AREA)

set.seed(34556638)
my.project =project[sample(1:nrow(project),100,replace=FALSE),]
summary(my.project)

hist(n_col+0.25)

boxplot(n_col, main = "Work Experience",
        xlab = "No of Years",
        ylab = "Level",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

mytable1 <- xtabs(~project$OFFENSE_CODE_GROUP + project$REPORTING_AREA + project$STREET, data=project)
ftable(mytable1)
summary(mytable1)


barplot(project$OFFENSE_CODE_GROUP, main = "Unergraduate Major", xlab = "Types of Majors", ylab = "No of Students", ylim = c(0,37443), xlim = c(0,70))


library(ggplot2)

ggplot(data = project$OFFENSE_CODE_GROUP, aes_string(x = "project$OFFENSE_CODE_GROUP", y = "n_col")) + 
  geom_boxplot()

head(n_col)
p1 <- ggplot(project, aes(x=project$OFFENSE_CODE_GROUP, y=n_col)) + geom_bar(stat="identity") + 
  labs(x="Percentage", y="Proportion")
plot(p1)

plot(n_col~factor(project$OFFENSE_CODE_GROUP), project, las=2, 
     xlab="", main="Mean Acceptability Ranking per Sentence")