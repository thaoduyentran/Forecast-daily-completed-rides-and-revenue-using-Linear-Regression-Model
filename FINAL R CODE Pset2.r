## MISM6202 - PROBLEM SET 2
## Thao Duyen Tran & Uyen Ha

########                  ########
##                              ##
##         DATA PREPPING        ##
##                              ##
#######                   ########


#data loading

cve <- read.csv("commutervan.csv")
sum(cve$ride==0) #2 days where there are 0 rides/0 revenue
#18 Jan 2016 is Martin Luther King day (federal holiday)
#15 Feb 2016 is Presidents' day (federal holiday)

dict <- read_xlsx("data dictionary.xlsx") #loading dictionary for reference

# Explore data
str(cve)
head(cve)
View(cve)

#checking NA/incorrect values
colSums(is.na(cve))
colSums(cve == '0')
colSums(cve=="NULL")

#removing days with 0 revenue/rides
cve <- cve[cve$rides>0,]
View(cve)



########                  ########
##                              ##
##      REGRESSION ANALYSIS     ##
##                              ##
#######                   ########


# QUESTION 1 

# regression for daily bookings (booked.ride) & completed rides (rides)
cve.fit <- lm(rides ~ booked.ride, data=cve)
summary(cve.fit)

ggplot(data=cve, aes(x=booked.ride, y=rides))+
  geom_point(color="#26823e")+
  labs(title="Plot of Daily Bookings and Daily Completed Rides",
       y="Number of Completed Rides (rides)",
       x="Number of Bookings (booked.ride)")+
  geom_smooth(method=lm, se=F, color="#91c294")


#regression for daily bookings (booked.ride) & completed rides in app (app.rides)
cve$app.rides = cve$booked.ride - cve$trip.cancelled

cve.fit2 <- lm(app.rides ~ booked.ride, data=cve)
summary(cve.fit2)

ggplot(data=cve, aes(x=booked.ride, y=app.rides))+
  geom_point(color="#26823e")+
  labs(title="Plot of Daily Bookings and Daily Completed Rides (in App)",
       y="Number of Completed Rides (app.rides)",
       x="Number of Bookings (booked.ride)")+
  geom_smooth(method=lm, se=F, color="#91c294")



## QUESTION 2 - regression of ride bookings on app usage stats
cve.fit3 <- lm(booked.ride~starts.session+tapped.sidebar+tapped.on.stop+viewed.eta,
                data=cve)
summary(cve.fit3)

#avg of starts.session compared to booked.ride
sum(cve$starts.session/cve$booked.ride)/nrow(cve)


#regression removing tapped.on.stop
cve.fit4 <- lm(booked.ride~starts.session+tapped.sidebar+viewed.eta,
               data=cve)
summary(cve.fit4)



########                  ########
##                              ##
##         FORECASTING          ##
##                              ##
#######                   ########


#Question 3 - scatter plot for rides

#rides~t
ggplot(data=cve, aes(x=t, y=rides))+
  geom_point(aes(color=dayofweek), cex=2)+
  labs(color="Day of the Week", title="Scatterplot for Daily Completed Rides Over Time",
       x="Time", y="Number of Completed Rides")+
  geom_smooth(method=lm, aes(col=dayofweek), se=F, cex=1)

#ride~month
ggplot(data=cve, aes(x=t, y=rides))+
  geom_point(aes(col=month))+
  labs(color="Month",
       title="Scatterplot for Daily Completed Rides Over Time 2",
       x="Time", y="Number of Completed Rides")

#rides~week total
cve$week <- cut(cve$t,
                breaks=seq(0,64,5), labels=seq(1,12,1))
cve$week <- factor(ifelse(is.na(cve$week),13,
                          cut(cve$t,
                              breaks=seq(0,64,5), labels=seq(1,12,1)))) 

week.sum <- aggregate(cve$rides, by=list(week=cve$week), FUN=sum)

cve$month <-  ifelse(cve$date>="2016-01-04" & cve$date <= "2016-01-29", "January",
          ifelse(cve$date>="2016-02-01" & cve$date <= "2016-02-29", "February",
          ifelse(cve$date>="2016-03-01" & cve$date <= "2016-03-31", "March",
                                "NULL")))

ggplot(data=week.sum, aes(x=week, y=x))+
  geom_point(col="#26823e", cex=2.5)+
  labs(color="Month",
    title="Scatterplot for Weekly Number of Completed Rides",
       x="Week", y="Total Number of Completed Rides")

#rides ~revenue total (colored by week)
week.rev <- aggregate(cve$revenue, by=list(week=cve$week), FUN=sum)
week.sum$revenue <- week.rev$x

ggplot(data=week.sum, aes(x=revenue, y=x))+
  geom_point(aes(color=week), cex=2.5)+
  labs(color="Week", 
       title="Scatterplot for Weekly Number of Completed Rides & Revenue",
       x="Total Revenue", y="Number of Completed Rides")


#rides ~ revenue colored by day of week
ggplot(data=cve, aes(x=revenue, y=rides))+
  geom_point(aes(color=dayofweek))+
  labs(color="Day of the Week", 
       title="Scatterplot for Daily Completed Rides & Revenue",
       x="Revenue", y="Number of Completed Rides")+
  geom_smooth(method=lm, se=F, col="#91c294")


#rides~app.rides
ggplot(data=cve, aes(x=app.rides, y=rides))+
  geom_point(aes(col=week), cex=2)+
  labs(title="Scatterplot for Daily Completed Rides (Overall) and Daily Completed Rides (in App)",
       x="Number of Completed Rides in the App", y="Number of Completed Rides Overall",
       col="Week")


#calculating how much revenue is generate per completed ride (weekly)
week.sum$rev.per.ride <- week.sum$revenue/week.sum$x

#rides~rev per ride
ggplot(data=week.sum, aes(x=rev.per.ride, y=x))+
  geom_point(aes(col=week), cex=2.5)+
  labs(color="Week",
       title="Scatterplot for Weekly Revenue per Number of Completed Rides",
       x="Revenue per Ride", y="Total Number of Completed Rides")

ggplot(data=week.sum, aes(y=rev.per.ride, x=week))+
  geom_point(col="#26823e", cex=2.5)+
  labs(title="Scatterplot for Weekly Revenue per Number of Completed Rides",
       x="Time", y="Revenue per Ride")



#QUESTION 4 k-period simple moving average

# Visually inspect the data for evidence of linear trend
ggplot(cve, aes(x=t, y=rides)) + 
  geom_line(color = '#91c294') + 
  labs(title = "Number of Completed Rides Over Time", x='Time (t)', 
       y='Number of Completed Rides') +
  geom_point(color="#26823e", size=1.7)+ 
  geom_abline(intercept=7, slope = 0.047)

#assign k values
k = 5   # 5 represents 5-day moving average


#making new column for moving average FOR 1 SIDE
moving_average <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
cve$five_day_avg <- moving_average(cve$rides)


#another method for moving average FOR 2 SIDES, NOT 1 SIDE
cve2$five_day_avg = rollmeanr(cve2$rides, k = 5, fill = NA)

# Report MSE, MAD (MAE), MAPE 
squared_error = (cve$rides -  cve$five_day_avg)^2
mean_square_error = mean(squared_error, na.rm=TRUE)
mean_square_error
abs_error= abs(cve$rides -  cve$five_day_avg)
mean_abs_error = mean(abs_error, na.rm = TRUE)
mean_abs_error
abs_percentage_error = ape(cve$ride, cve$five_day_avg)
mean_abs_percentage_error = mean(abs_percentage_error, na.rm=TRUE)
mean_abs_percentage_error


# Visualize Time Series - 5 days Moving Average
library(tidyquant)
ggplot(cve, aes(x=t, y=rides)) + geom_line(color = '#91c294') + 
  geom_ma(ma_fun= SMA, n=5, size =.4, color = 'black', show.legend = T)+
  labs(title = "Number of Completed Rides Over Time with 5-Day Moving Average ", 
       x='Time Period Index (t)', 
       y='Number of Completed Rides') +
  geom_point(color="#26823e", size=1.5)



## QUESTION 5 - Estimate a linear trend model for the 'rides' variable
rides.model <- lm(rides~t, data=cve)
summary(rides.model)

#plotting
plot(rides~t, data=cve)
abline(rides.model)

#plotting with ggplot
ggplot(data=cve, aes(x=t, y=rides))+
  geom_point(aes(color=week))+
  labs(title="Linear Trend Model for Completed Rides over Time",
       x="Time Period Index (t)",
       y="Number of Completed Rides (rides)",
       col="Week")+
  geom_smooth(method="lm", se=F, col="#91c294")



## QUESTION 6 - Estimate a linear trend model 
##              with day-of-week dummy variables for the ‘rides’ variable. 

# Create seasonal dummy variables for 4 of the 5 seasonalities 
cve$d1 <- ifelse(cve$dayofweek == "Monday", 1, 0)
cve$d2 <- ifelse(cve$dayofweek == 'Tuesday', 1, 0)
cve$d3 <- ifelse(cve$dayofweek == "Wednesday", 1, 0)
cve$d4 <- ifelse(cve$dayofweek == "Thursday", 1, 0)

# estimation model with dummy variables
dummy.model <- lm(rides ~ d1 + d2 + d3 + d4 + t, data = cve)
summary(dummy.model)


# storing coefficients
b0.int <- dummy.model$coefficients[1]
b1.mon <- dummy.model$coefficients[2]
b2.tue <- dummy.model$coefficients[3]
b3.wed <- dummy.model$coefficients[4]
b4.thu <- dummy.model$coefficients[5]



## QUESTION 7A - Use the estimated regression equation from (6) 
#                to calculate a forecast of ‘rides’ for each day in your dataset. 

# Perform prediction
dummy.pred <- predict(dummy.model, cve, type = 'response')
dummy.pred 


# Install required packages for analysis
install.packages('smooth')
install.packages('forecast')
library(forecast)
library(smooth)
install.packages('Mcomp')
library(Mcomp)
library(Metrics)

# Calculate MSE, MAD, and MAPE for this forecast

MSE6 = mse(cve$rides, dummy.pred)
MSE6

MAD6 = mae(cve$rides, dummy.pred)
MAD6

MAPE6 = mape(cve$rides, dummy.pred)
MAPE6

#calculate by hand
squared_error2 = (cve$rides -  dummy.pred)^2
mean_square_error2 = mean(squared_error2, na.rm=TRUE)
mean_square_error2


abs_error2= abs(cve$rides -  dummy.pred)
mean_abs_error2 = mean(abs_error2, na.rm = TRUE)
mean_abs_error2

abs_percentage_error2 = ape(cve$ride, dummy.pred)
mean_abs_percentage_error2 = mean(abs_percentage_error2, na.rm=TRUE)
mean_abs_percentage_error2




## QUESTION 7B - Use the estimated regression equation from (7a) 
#                to forecast daily completed rides 
#                for each weekday in the next month 

#Create new data set for the month of April 
library("lubridate")

## Create required independent variables for prediction
### 1-Apr to 29-Apr is 21 days (exclude Saturday, Sunday for 4 weeks)
t = 63:83
dayofweek = cve$dayofweek[5:25] #start with Friday
rides = NA 
revenue = NA 
newdata = data.frame(t, dayofweek, rides, revenue)

newdata$d1 <- ifelse(newdata$dayofweek == "Monday", 1, 0)
newdata$d2 <- ifelse(newdata$dayofweek == 'Tuesday', 1, 0)
newdata$d3 <- ifelse(newdata$dayofweek == "Wednesday", 1, 0)
newdata$d4 <- ifelse(newdata$dayofweek == "Thursday", 1, 0)

newdata$month = 'April'

# Perform prediction of rides using dummy model on new data for April 
new.pred = predict(dummy.model, newdata, type = 'response')
newdata$rides = new.pred 


# Model for revenue prediction 
rev.model = lm(revenue ~ rides + d1 + d2 + d3 + d4 + t, cve)
summary(rev.model)

rev.model2 = lm(revenue ~ rides + t, cve)
summary(rev.model2)

# Forecast revenue 
newdata$revenue = predict(rev.model, newdata, type = 'response')

# Combine new data and cve 
library(dplyr)

combined_data = data.frame(cve$month, cve$rides, cve$revenue)

combined_data = combined_data %>% 
  add_row(cve.month = newdata$month, cve.rides = newdata$rides,
                          cve.revenue = newdata$revenue)
combined_data$t = 1:83


# Visualize the predictions for completed rides 
library(ggplot2)

#rides~t (month)
ggplot(data=combined_data, aes(x=t, y=cve.rides))+
  geom_point(aes(col=cve.month))+
  geom_smooth(method="lm", se=F, color="#91c294")+
  labs(color="Month",
       title="Scatterplot for Daily Completed Rides Prediction Over Time (Jan to Apr)",
       x="Time Series Index (t)", y="Number of Completed Rides")


#rides~t (dayofweek)
dayofweek_column <- c(cve$dayofweek, newdata$dayofweek)
combined_data$dayofweek <- dayofweek_column


ggplot(data=combined_data, aes(x=t, y=cve.rides))+
  geom_point(aes(col=dayofweek))+
  geom_smooth(method="lm", se=F, aes(col=dayofweek), size=0.8)+
  labs(color="Day of Week",
       title="Scatterplot for Daily Completed Rides Prediction Over Time (Jan to Apr) 2",
       x="Time Series Index (t)", y="Number of Completed Rides")


# Visualize the predictions for revenue 

#revenue~t (month)
ggplot(data=combined_data, aes(x=t, y=cve.revenue))+
  geom_point(aes(col=cve.month))+
  geom_smooth(method="lm", se=F, color="#91c294")+
  labs(color="Month",
       title="Scatterplot for Revenue Prediction Over Time (January to April)",
       x="Time", y="Revenue")

#revenue~t(dayofweek)
ggplot(data=combined_data, aes(x=t, y=cve.revenue))+
  geom_point(aes(col=dayofweek))+
  geom_smooth(method="lm", se=F, aes(col=dayofweek), size=0.8)+
  labs(color="Day of Week",
       title="Scatterplot for Revenue Prediction Over Time (January to April)",
       x="Time", y="Revenue")


## QUESTION 8 - additional plot

dummy.model2 <- lm(rides ~ d1 + d2 + d3 + d4 + t + week, data = cve)
summary(dummy.model2)

# Perform prediction
dummy.pred2 <- predict(dummy.model2, cve, type = 'response')
dummy.pred2

MSE8 = mse(cve$rides, dummy.pred2)
MSE8

MAD8 = mae(cve$rides, dummy.pred2)
MAD8

MAPE8 = mape(cve$rides, dummy.pred2)
MAPE8


#################################


## QUESTION 6 - Estimate a linear trend model 
##              with day-of-week dummy variables for the ‘rides’ variable. 

# Visually inspect the data for evidence of linear trend
library(ggplot2)
ggplot(cve, aes(x=t, y=rides)) + geom_line(color = 'darkblue') + 
  labs(title = "Number of completed Rides Over Time ", x='Time period index t', 
       y='The number of completed rides') +
  geom_point(color="red", size=1.5)+ 
  geom_abline(intercept=7, slope = 0.047) + 
  theme_classic() 


# Create seasonal dummy variables for 4 of the 5 seasonalities 
cve$d1 <- ifelse(cve$dayofweek == "Monday", 1, 0)
cve$d2 <- ifelse(cve$dayofweek == 'Tuesday', 1, 0)
cve$d3 <- ifelse(cve$dayofweek == "Wednesday", 1, 0)
cve$d4 <- ifelse(cve$dayofweek == "Thursday", 1, 0)




## Split data into test & train
set.seed(1)  # Set seed so that same sample can be reproduced in future
## Selecting 80% of data for training
sample <- sample(1:nrow(cve), size=floor(0.80*nrow(cve)), replace=FALSE)
train <- cve[ sample, ]
test  <- cve[-sample, ]

# Estimate the multiple regression model with time trend + seasonal dummy variables
## 5-period Simple Moving Average 
model <- lm(rides ~ d1 + d2 + d3 + d4 + t, data = train)
summary(model)

# Perform prediction on test set 
pred = predict(model, test, type = 'response')
pred 

# Calculate model performance MSE, MAD (MAE), and MAPE
MSE = mse(test$rides, pred)
MSE
MAD = mae(test$rides, pred)
MAD
MAPE = mape(test$rides, pred)
MAPE

library(zoo)  
library(tidyverse)
library(dplyr)
library(tidyverse)
library(caret)
library('e1071')
library('kernlab')

# The k-Fold cross-validation
set.seed(100)
# Perform 10 fold cross validation
trctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
nb_fit <- train(rides ~ d1 + d2 + d3 + d4 + t, data = train, method = "lm", 
                trControl=trctrl, tuneLength = 0)
# Check model performance
nb_fit


