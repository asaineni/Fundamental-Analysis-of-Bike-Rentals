
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip","Bike.zip")

day <- read.table(unz("Bike.zip", "day.csv"), header=T, quote="\"", sep=",")

library(dplyr)
library(heuristica)
library(corrplot)
library(forcats)
library(ggplot2)
library(DataExplorer)
library(caret)
#standardise the temp and atemp

day$temp<- day$temp*41
day$atemp<- day$atemp*50
day$hum<- day$hum*100
day$windspeed<-day$windspeed*67
########################################################################################
#look at the data

dim(day)
str(day)
summary(day)
glimpse(day)
names(day)

#Look st the data

plot_histogram(day)

#Distribution of rental counts
par(mfrow=c(1,1))
hist(day$cnt, probability = TRUE, breaks = 25, col = "skyblue")  
x<- 0:10000
y<- dnorm(x = x, mean = mean(day$cnt), sd(day$cnt))
lines(x = x, y = y, col = "red")

#Data is normally distributed

table(is.na(day))


#Plotting Correlation of response variable with explanatory varibales.
nonums <- unlist(lapply(day, is.numeric))  
nums<-day[,nonums]

par(mfrow=c(1,1))
corrplot(cor(nums))
symnum(cor(nums))

###Distribution of numeric variables


par(mfrow=c(2,2))

plot(day$cnt~day$temp ,type = 'p', col= 'violetred', xlab = 'Temperature', ylab = 'Total Count')
abline(lm(day$cnt~day$temp))

plot(day$cnt~day$atemp ,type = 'p', col= 'royalblue', xlab = 'Feels Like Temp', ylab = 'Total Count')
abline(lm(day$cnt~day$atemp))

plot(day$cnt~day$windspeed ,type = 'p', col= 'lightsalmon3', xlab = 'Windspeed', ylab = 'Total Count')
abline(lm(day$cnt~day$windspeed))

plot(day$cnt~day$hum ,type = 'p', col= 'darkslategray4', xlab = 'Humidity', ylab = 'Total Count')
abline(lm(day$cnt~day$hum))

###################################################
#Distribution of categorical variables
#Transform Catagorical Variables
day$season<- factor(day$season
                    ,levels = c(1,2,3,4)
                    ,labels = c("spring", "summer", "fall", "winter")
)


day$weathersit<-factor(day$weather
                       ,levels = c(3,2,1)
                       ,labels = c("Bad", "Normal", "Good")
                       ,ordered = TRUE)

day$holiday<- factor(day$holiday
                     ,levels = c(0,1)
                     ,labels = c("noholiday", "holiday")
)

day$workingday<-factor(day$workingday
                       ,levels = c(0,1)
                       ,labels = c("nonworking", "working")
)

day$weekday<-factor(day$weekday
                    ,levels = c(0,1,2,3,4,5,6)
                    ,labels = c("sun", "mon","tue","wed","thur","fri","sat")
)

###Plot Categorical variables
ggplot(day, aes(x = fct_infreq(season), y = cnt, fill = season))+
  geom_bar(stat = "identity")+
  labs(title = "Bike count vs Season",
       x = "Season", y = "Count of bikes") +
  theme(legend.position = "right")

ggplot(day, aes(x = fct_infreq(holiday), y = cnt, fill = holiday))+
  geom_bar(stat = "identity")+theme_minimal()+
  labs(title = "Bike count vs Holiday",
       x = "holiday", y = "Count of bikes") +
  theme(legend.position = "right")

ggplot(day, aes(x=fct_infreq(weathersit), y=cnt, fill=weathersit)) +
  geom_bar(stat="identity")+theme_minimal()+
  labs(title = "Bike count vs Weather Condition",
       x = "Weather", y = "Count of bikes") +
  theme(legend.position = "right")

ggplot(day, aes(x=fct_infreq(workingday), y=cnt, fill=workingday)) +
  geom_bar(stat="identity")+theme_minimal()+
  labs(title = "Bike count vs Working Day",
       x = "Workingday", y = "Count of bikes") +
  theme(legend.position = "right")

###################################################
#Working, nonworking vs casual, registered
ggplot(day, aes(x = casual, y = registered, color = workingday))+
  geom_point()+
  labs(title = "Relation Between Bike counts(casual& registeres) vs Working, Non working")+
   scale_color_manual(values=c("deeppink", "turquoise2")) +
  xlab("Casual Bike Counts") +
  ylab("Registered Bike Counts")


####################################################
#monthly bike trends of registered and casual bikers.
par(mfrow=c(1,1))

boxplot(day$cnt~day$mnth,xlab="mnth", ylab="count of users", col= "violetred4", 
        main = "Monthly Bike Users")

par(mfrow=c(1,2))
boxplot(day$registered~day$mnth,xlab="mnth", ylab="registered users", col= "lightpink1",
        main = "Montly Registered Users")
boxplot(day$casual~day$mnth,xlab="mnth", ylab="casual users", col= "lightskyblue3",
        main = "Monthly Casual Users")

####################################################
#Casual VS Registered bikers
cr <- aggregate(. ~ mnth
                ,data = day[c("casual"
                                ,"registered"
                                ,"mnth")]
                ,sum)

rownames(cr) <- cr$mnth

cr <- cr[c("casual", "registered")]
print(cr)

######################################################
#Daily trends
date=substr(day$dteday,1,10)
days<-weekdays(as.Date(date))
day$days=days

par(mfrow=c(1,1))
boxplot(day$cnt~day$days,xlab="days", ylab="count of users", col = "steelblue4",
        main = "Daily Bike Users")


#################Plotting Correlation between temperature and bike rentals(casual & registered) 
plot(x = 1, y = 1, xlab = "Temperature in Celsius", ylab = "Bike rentals", 
     type = "n", main = "Association between temperature and bike rentals",
     xlim = c(0, 40), ylim = c(0, 7000))

#adding points to the plot
points(day$temp, day$casual, pch = 16, col = "lightgreen")
points(day$temp, day$registered, pch = 16, 
       col = "skyblue")
abline(lm(day$registered~day$temp))
abline(lm(day$casual~day$temp))

legend("topleft",legend = c("casual", "registered"), col = c("lightgreen","skyblue"),
       pch = c(16, 16), bg = "transparent")

#Running Correlation between normalized temperature in both registered and casual users.
#since the p-value is less than 5% for both registered and casual users, the 
# the relationship between temperature and number of users(both casual and 
# registered) is significant. The number of users is positively correlated to
# the temperature.
cor_registered <- cor.test(x = day$temp, 
                           y = day$registered)
cor_registered

cor_casual <- cor.test(x = day$temp, 
                       y = day$casual)
cor_casual

registered_users = paste("cor = ", round(cor(day$temp, day$registered), 2),sep = "")
casual_users = paste("cor = ", round(cor(day$temp, day$casual), 2), sep = "")

legend("left", legend = c(registered_users,casual_users), 
       col = c("skyblue", "lightgreen"), 
       pch = c(16, 16), bg = "transparent")

###########################
#The graph shows that there is a positive correlation between temperature and 
#bike rentals i.e. as the temperature increases the number of bike rentals also 
#increases and vice versa. Importantly, the number of registered users is way 
#more than number of casual users with respect to temperature.


#######################################################################################

#prepare data for linear regression##############################
#we will remove casual, registered, dteday, and instrant from data to do linear
#regression. casual and registered included in cnt and dteday is not a single independent 
#variable.

day$instant<-NULL
day$dteday<- NULL
day$casual<-NULL
day$registered<- NULL

#Transform workingday and holiday as numeric variable
day$holiday<- as.numeric(day$holiday)
day$workingday<- as.numeric(day$workingday)

#Data manipulating
#Transform Month into quarters
day$Quarter <- ceiling(as.numeric(day$mnth) / 3)
day$Quarter<- factor(day$Quarter)
day$mnth = NULL

library(ade4)
#Dummy variables for factor variable
factor_variables <- sapply(day,is.factor)
day_factor <- day[,factor_variables]

factor.names <- names(day_factor)
day_factor <- as.data.frame(day_factor)
day_factor <- acm.disjonctif(day_factor)

###############merge final dataset##################################
day <- day[,-which(names(day) %in% factor.names)]

day <- cbind(day,day_factor)

rm(day_factor,factor_variables,factor.names)

nums <- unlist(lapply(day, is.numeric))  
day<-day[,nums]

day$cnt<- as.numeric(day$cnt)
day$yr<- as.factor(day$yr)
str(day)

#again transform holiday and workingday as factor for modeling
day$holiday<- as.factor(day$holiday)
day$workingday<- as.factor(day$workingday)

#Final Data for Modeling is ready. Before modeling we will check the assumptions
#1 Linearity
linear<- lm(cnt~ ., data = day)
summary(linear)
#Create standarized residuals and plot linearity 
standardized = rstudent(linear) 
qqnorm(standardized) 
abline(0,1) 

#2 Normality

hist(standardized, breaks = 15)
mean(linear$residuals)

#3Homogeneity/Homoscedasticity
fitvalues = scale(linear$fitted.values)
plot(fitvalues, standardized)
abline(0,0) 
abline(v = 0) 

plot(linear, 1)


########################split into train and test######################

set.seed(123)
smp_size <- floor(0.75 * nrow(day))
train_ind <- sample(seq_len(nrow(day)), size = smp_size)

train <- day[train_ind, ]
test <- day[-train_ind, ]
##################################################################

#Build Model Linear regression.
#Linear regression
#building a model without the  date, casual, registered and instant
#as the cnt variable includes both casual and registered and the dteday variable is not a independent variable, 
#but consist variable that overlap with variables such as month, weeking day, holiday
model1<- lm(cnt ~ temp +atemp+ hum +windspeed, data = train)
summary(model1)
prediction1<- predict(model1, newdata = test)
mean((prediction1 - test$cnt)^2)
RMSE(prediction1, test$cnt)
R2(prediction1, test$cnt)
AIC(model1)
BIC(model1)
confint(model1)

model2<- lm(cnt~ temp+ hum+ windspeed, data= day)
summary(model2)
prediction2<- predict(model2, newdata = test)
mean((prediction2- test$cnt)^2)
RMSE(prediction2, test$cnt)
R2(prediction2, test$cnt)
AIC(model2)
BIC(model2)
confint(model2)


model3<- lm(cnt~ .-atemp, data = day)
summary(model3)
prediction3<- predict(model3, newdata = test)
mean(( prediction3- test$cnt)^2)
RMSE(prediction3, test$cnt)
R2(prediction3, test$cnt)
AIC(model3)
BIC(model3)
confint(model3)


model4<- lm(cnt~ .- atemp- hum- windspeed, data = train)
summary(model4)
prediction4<- predict(model4, newdata = test)
mean(( prediction4- test$cnt)^2)
RMSE(prediction4, test$cnt)
R2(prediction4, test$cnt)
AIC(model4)
BIC(model4)
confint(model4)

  
model5<-lm(cnt~ .-atemp-weekday.wed-weekday.thur-weekday.fri-
             weekday.mon-weekday.tue-weekday.wed-workingday
           -weathersit.Good-Quarter.4, data = train)
summary(model5)
prediction5<- predict(model5, newdata = test)
mean((prediction5 -test$cnt)^2)
RMSE(prediction5, test$cnt)
R2(prediction5, test$cnt)
AIC(model5)
BIC(model5)
confint(model5)

#Interpretation

#plot residuals
par(mfrow= c(2,2))
plot(model5, col = "gold")

#Model Accuracy Assesment
#Predicting using the attributes from testing dataset and plot them against the true 
#values the graph shows that the spread of the response variable is similar to multilinear
#model. Still, we connot depend on this because we worked on a small data and this dataset
#does not contain daily hours and bike stations, which can help more in accuracy.

par(mfrow= c(1,1))
model5_step<- step(model5)
plot(test$cnt, main = "Linear Model", ylab = "Test Set Rental Count", pch = 20)
points(predict(model5_step, newdata = test), col = "red", pch = 20)







