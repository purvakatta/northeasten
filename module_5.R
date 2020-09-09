ldeaths

l_ts <- ts(ldeaths, start = c(1974,1), frequency = 12)
plot(l_ts, main="Time Series Plot", ylab = "Deaths", lwd="2", col="green")     ### Fig. 1
text(ldeaths)

monthplot(l_ts, main="Monthly Plot for Deaths", ylab = "No. of Deaths")        ### fig. 2

######################################################################################

l_dec <- decompose(l_ts)
plot(l_dec, lwd="2", col="blue")                                         #### fig. 3


######################################################################################

l_sa <- l_ts - l_dec$seasonal
l_sa                                                                      ##### fig. 4


plot(l_sa, main="Time series Seasonal Adjustment", ylab="Deaths Seasonal Adjustment", lwd="2", col="brown")



hist(l_sa, # histogram
     col="lightblue", # column color
     border="darkblue",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Deaths", 
     main = "Seasonal Adjusted Histogram")
lines(density(l_sa), # density plot
      lwd = 2, # thickness of line                                         #### Fig. 5
      col = "orange")

############################################################################################


lres <- stl(ldeaths, s.window = "periodic")
lres
plot(lres)                                                                     ### Fig. 6
summary(lres)

#############################################################################################

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

############################################################################################
install.packages("astsa")

library(astsa)

gtemp_land

gts <- ts(gtemp_land, start = c(1880,1), frequency = 1)
plot(gts, main="Time Series Plot", ylab = "Average Temperature - Land", lwd="2", col="dark green")  ###Fig. 7

#######################################################################################################

library(lmtest)


r <- lm(gtemp_land ~ ., data = gtemp_land)
summary(r)
dwtest(r)


Acf(gtemp_land)                 ############## Fig. 9      
Acf(gtemp_land, plot = FALSE)

Pacf(gtemp_land, plot = FALSE)
Pacf(gtemp_land)   ############### Fig. 10

################################################################################

library(forecast)

sem <- ses(gtemp_land, h=10)
sem
plot(sem)                                    ### Fig. 8
accuracy(sem)


summary(sem)
sem$model

##########################################################################################


arm <- auto.arima(gtemp_land)
summary(arm)


test <- acf(resid(arm))                    #### Fig. 11
summary(test)

gstf <- forecast(gtemp_land)
autoplot(gstf, ylab = "Temperature on Land")                 ##### Fig. 12



par(mfrow=c(2,1))				
Acf(arm$residuals)                           
Acf(arm$fitted)                             #### Fig. 13



ts.plot(cbind(gts, arm$residuals), lty=1:2, col=c(1,2), main="Residual ARIMA TS Plot")     ## Fig. 14

ts.plot(cbind(gts, arm$fitted), lty=1:2, col=c(1,2), main="Fitted ARIMA TS Plot")                 ### Fig. 15



reg <- Arima(gts,order=c(1,1,0),seasonal=c(1,0,0))		
reg
fc <- forecast(gts,model=reg)		
fc

autoplot(fc)                                                         ####Fig. 16




















