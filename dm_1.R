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
cor(kc_data$price, kc_data$sqft_lot, method = c("pearson"), use = 'complete.obs') 


cor(kc_data$price, kc_data$floors, method = c("pearson"), use = 'complete.obs') 


cor(kc_data$bedrooms, kc_data$sqft_lot, method = c("pearson"), use = 'complete.obs') 



############## making a subset for integer values to check correlation

some.data <- subset(kc_data, select = c("price","view","floors", "bedrooms", "bathrooms",
                                       "condition",
                                       "grade",
                                       "waterfront", "yr_built", "yr_renovated"))

x <- cor(some.data, method = c("pearson"))

ggcorrplot(x, type = "lower", lab = TRUE)

################################# visualization of correlation

library(GGally)

ggpairs(data=kc_data, columns=3:12,
        mapping = aes(color = "dark green"),
        axisLabels="show")


######################################################################################



#####################################################################################################
########################## visualization ##############################################################


library(ggplot2)
library(ggcorrplot)
library(scales)


ggplot(data = kc_data , aes(x = price)) +
  geom_density(fill = 'cyan')  +
  scale_x_continuous(name = "price", labels = comma)

#################

priceper_sqft <- kc_data$sqft_lot/kc_data$price


ggplot(kc_data, aes(x = date, y = priceper_sqft )) +
  geom_line() +
  labs(title = "Price per Square/ft per Year",
       x = "Year",                                     ### price/sqft vs date
       y = "Price")


############################################

ggplot(kc_data, aes(x = date, y = price)) + 
  geom_line(color = "darkblue") +                                         ### price vs date
  ggtitle("Houses prices series") + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date( date_breaks = "2 months")


##################################################################################################

library(scales)

boxplot(price~sqft_living, data= kc_data, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price") +
  scale_y_continuous(name="price", labels = comma)

plot(kc_data$sqft_living,kc_data$price, 
     main="Sqft_Living vs.
     House Price", xlab="Sqft_Living", ylab="Price of House", 
     pch=19, col = "blue")

###############################


plot(kc_data$sqft_lot,kc_data$price, 
     main="Sqft_Living vs. Price of House", xlab="Sqft_Lot", ylab="House Price", 
     pch=19, col = "darkgreen") + scale_y_continuous(name="price", labels = comma)

 
#########################################################################################
ggplot(data = kc_data) +
  geom_histogram(mapping = aes(x = grade), binwidth = 0.5)      ### houses vs grade

boxplot(price~grade, data=kc_data, 
        col=(c("gold","darkgreen")),
        main="Price vs. Grade", xlab="Grade", ylab="Price")

###############################################

boxplot(price~view, data=kc_data, 
        col=(c("gold","darkgreen")),
        main="Price vs. View", xlab="View", ylab="Price")

######################################################

with(kc_data,
     lines(yr_built, yr_renovated,
          main="",
          xlab="year built", ylab="year renovated",
          pch=20, cex=0.7, col="darkgreen")
)

######################################################


ggplot(kc_data, aes(x = date, y = price)) +
  geom_line(color = "indianred3", 
            size=1 ) +                                    ### price vs date to see the trend
  geom_smooth() +
  scale_x_date(date_breaks = '30 days', 
               ) +
  labs(title = "House Prices",
       subtitle = "2014 to 2015",
       x = "",
       y = "Price") +
  theme_minimal()


####################################################boxplots for price with year built and renovated

ggplot(kc_data, aes(x= yr_renovated, y=price)) + 
  geom_boxplot(notch = TRUE) +
  geom_jitter(position=position_jitter(0.2)) + scale_y_continuous(name="price", labels = comma)

ggplot(kc_data, aes(x= yr_built, y=price)) + 
  geom_boxplot(notch = TRUE) +
    scale_y_continuous(name="price", labels = comma)



skewness(kc_data$price, na.rm = FALSE)

#########################################################################
###finding out important variables in dataset


ggplot(data = kc_data ) +                                    ### houses vs condition 
  geom_bar(mapping = aes(x = condition)) 


kc_data %>% 
  count(kc_data$condition) 
  

boxplot(price~condition, data=kc_data, 
        col=(c("gold","darkgreen")),
        main="Price vs. Grade", xlab="Grade", ylab="Price") 
+scale_y_continuous(name="price", labels = comma)

##############################################################



     ## price of houses vs no of bedrooms

 boxplot(bathrooms~price, data=kc_data, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Bathrooms", xlab="Price", ylab="Bathrooms")

###################################################################


kc_new <- kc_data %>%
  group_by(date, price, yr_built, yr_renovated) %>%
  tally() %>%
  collect() %>%
  arrange(date)

#######################
new_kc <- kc_data %>% 
       group_by(price, yr_built, yr_renovated) %>%
       filter(!yr_renovated == "0") %>%
       tally() %>%
         collect() 

ggplot() + geom_point(data = kc_data , aes(x = yr_built, y = price, color = yr_renovated)) +
  scale_y_continuous(name="price", labels = comma)

########################################



ggplot(kc_data, aes(x = sqft_lot, y = price )) +
  geom_line() +
  labs(title = "Price per Square Feet",
       x = "Square Feet",
       y = "Price") + scale_y_continuous(name="price", labels = comma)

##############################################################################################3
########### price trendsd according to area

boxplot(price~lat, data=kc_data, 
        col=(c("gold","darkgreen")),
        main="Price vs. Lat", xlab="Lat", ylab="Price")




