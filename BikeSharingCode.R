library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)

url.hr <- "https://raw.githubusercontent.com/nitishghosal/Anomaly-Detection-Demand-Forecasting/master/hour.csv"

data.hr <- as_data_frame(read.csv(url.hr, stringsAsFactors = FALSE))

url.day<-"https://raw.githubusercontent.com/nitishghosal/Anomaly-Detection-Demand-Forecasting/master/day.csv"

data.day<- as_data_frame(read.csv(url.day, stringsAsFactors = FALSE))

class(data.hr)
class(data.day)

colnames(data.hr)
colnames(data.day)

dim(data.hr)
dim(data.day)

str(data.hr)
str(data.day)

#Check for missing values
sapply(data.hr, function(x) sum(is.na(x)))
sapply(data.day, function(x) sum(is.na(x)))

#Convert Columns to Factors
colname_factor <- c('season', 'yr', 'holiday',
                    'workingday', 'weathersit')
data.hr[,colname_factor] <- lapply(data.hr[,colname_factor], factor)
data.day[,colname_factor] <- lapply(data.day[,colname_factor], factor)

str(data.day)
str(data.hr)

#Check Correlation

cor(data.hr[,-c(1:10)])
cor(data.day[,-c(1:10)])

corrgram::corrgram(data.hr[,-c(1:10)], main="Bike Sharing Hourly Data (alphabetic order)")
corrgram::corrgram(data.day[,-c(1:10)], main="Bike Sharing Daily Data (alphabetic order)")

#Boxplot to detect outliers

par(mfrow=c(2,3))
title("Boxplots for Hourly Data")
boxplot(data.hr$cnt)
boxplot(data.hr$registered)
boxplot(data.hr$casual)
boxplot(data.hr$windspeed)
boxplot(data.hr$hum)
boxplot(data.hr$atemp)

par(mfrow=c(2,3))
title("Boxplots for daily Data")
boxplot(data.day$cnt)
boxplot(data.day$registered)
boxplot(data.day$casual)
boxplot(data.day$windspeed)
boxplot(data.day$hum)
boxplot(data.day$atemp)

#############################################################################

library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)


data.hr$season  <- factor(data.hr$season, labels = c("Spring", "Summer", "Fall", "Winter"))
data.hr$weathersit <- factor(data.hr$weathersit, labels = c("Good", "Normal", "Bad", "Very Bad"))
data.hr$hr<- as.factor(data.hr$hr)
data.hr$weekday<-factor(data.hr$weekday,labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

season_summary <- ddply(data.hr,.(season,hr),
                        summarise, count = mean(cnt))

glimpse(season_summary)

## summary statistcs from the numerical variables...
str(data.hr)
library("DT")
bike_summary <-data.hr %>%
  select(temp,atemp,hum, windspeed,registered, casual, cnt)



my.summary <- function(x, na.rm=TRUE){
  result <- c(Mean = format(round(mean(x, na.rm = na.rm),digits = 2)),
              SD = format(round(sd(x, na.rm = na.rm), digits = 2)),
              Median = format(round(median(x, na.rm = na.rm), digits = 2)),
              Min = format(round(min(x, na.rm = na.rm), digits = 2)),
              Max = format(round(max(x, na.rm = na.rm), digits = 2)), 
              N_Obs = length(x),
              N_NA = sum(is.na(x)))
}
summary_bike <- sapply(bike_summary, my.summary)

datatable(summary_bike, class = 'cell-border stripe', options = list(dom = 't')) 

### summary statistics and distribution of the categorical variables
#View(data.hr)
par(mfrow=c(2,3))
data.hr$holiday<-factor(data.hr$holiday,labels = c("non-holiday","holiday"))
barplot(table(data.hr$holiday)/sum(table(data.hr$holiday)),main = "Holiday")
abline(h=1/length(levels(data.hr$holiday)))

barplot(table(data.hr$season)/sum(table(data.hr$season)),main = "Season")
abline(h=1/length(levels(data.hr$season)))

# barplot(table(data.hr$mnth))
data.hr$yr<-factor(data.hr$yr,labels = c("2011","2012"))
barplot(table(data.hr$yr)/sum(table(data.hr$yr)),main = "Year")
abline(h=1/length(levels(data.hr$yr)))

data.hr$weekday<-factor(data.hr$weekday,labels = c("Su","Mo","Tu","We","Th","Fr","Sa")) # I changed the labels to fit them in the visualization
barplot(table(data.hr$weekday)/sum(table(data.hr$weekday)),main = "Weekday")
abline(h=1/length(levels(data.hr$weekday)))

data.hr$workingday<-factor(data.hr$workingday,labels = c("non-workingday","workingday"))
barplot(table(data.hr$workingday)/sum(table(data.hr$workingday)),main = "Workingday")
abline(h=1/length(levels(data.hr$workingday)))

barplot(table(data.hr$weathersit)/sum(table(data.hr$weathersit)),main = "Weathersit")
abline(h=1/length(levels(data.hr$weathersit)))

# back to regular labels for weekday
data.hr$weekday<-factor(data.hr$weekday,labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#### visualizations

## histogram of the response variable

# histogram of the response variable 'count'
ggplot(data.hr, aes(cnt)) +
  geom_histogram(binwidth = 5) + 
  geom_vline(data=data.hr, aes(xintercept = median(data.hr$cnt)), colour="red") +
  theme_minimal()+
  ggtitle("Distribution of the response variable 'count' ",subtitle = "Half of the observations have a 'count' of less than 142") + 
  theme(plot.title=element_text(size=18))

## visualization of the predictor variables

ggplot(data.hr, aes(x = hr, y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike shares are most popular in Fall and least in Spring.\n") + 
  theme(plot.title=element_text(size=18))

weather_summary <- ddply(data.hr,.(weathersit,hr),
                         summarise, count = mean(cnt))
glimpse(weather_summary)

ggplot(data.hr, aes(x = hr, y = count, colour = weathersit)) +
  geom_point(data = weather_summary, aes(group = weathersit)) +
  geom_line(data = weather_summary, aes(group = weathersit)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike sharing is most popular when the weather is 'Good'\n") + 
  theme(plot.title=element_text(size=18))


day_summary <- ddply(data.hr,.(weekday,hr),
                     summarise, count = mean(cnt))
ggplot(data.hr, aes(x = hr, y = count, colour = weekday)) +
  geom_point(data = day_summary, aes(group=weekday)) +
  geom_line(data = day_summary, aes(group=weekday)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes for morning/evening commutes on weekdays and daytime rides on weekends\n")


weather_prob <- ddply(data.hr,.(season, hr),
                      summarise, Good = mean(weathersit == "Good"),
                      Normal = mean(weathersit == "Normal"),
                      Bad = mean(weathersit == "Bad"),
                      Very_bad = mean(weathersit == "Very Bad"))


ggplot(data.hr, aes(x = hr, y = Good, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Good Weather") +
  theme_minimal() +
  ggtitle("The probability of Good weather is higher in Fall. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(data.hr, aes(x = hr, y = Normal, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Normal Weather") +
  theme_minimal() +
  ggtitle("The probability of Normal weather is higher in Winter. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(data.hr, aes(x = hr, y = Bad, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Bad Weather") +
  theme_minimal() +
  ggtitle("The probability of Bad weather is higher in Summer and Winter. \n") + 
  theme(plot.title=element_text(size=18))

############################## final viz that reinforces the weather and the hour...
par(mfrow=c(1,1))
plot(data.hr$cnt, data.hr$windspeed)
str(data.hr)
ggplot(data.hr, aes(x=atemp, y=cnt,col = workingday)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE )+
  facet_grid(~workingday)+
  theme_minimal() +
  ggtitle("The popularity of the rentals increse with higher temperatures", subtitle = "the influence of the temperature on bike rentals is higher during non-working days") + 
  theme(plot.title=element_text(size=18))


ggplot(data.hr, aes(x=atemp, y=cnt)) +
  geom_bar(mapping = aes(fill = workingday),stat = "identity", position = "dodge")+
  geom_smooth(method=lm, se=FALSE )+
  theme_minimal() +
  ggtitle("The popularity of the rentals increse with higher temperatures", subtitle = "the influence of the temperature on bike rentals is higher during non-working days") + 
  theme(plot.title=element_text(size=18))

#########################################################################

HourCasualRegistered <- aggregate(data.hr[,c("casual","registered")], by=list(data.hr$hr), "sum")
colnames(HourCasualRegistered) <- c('Hour','casual','registered')

suppressWarnings(suppressMessages(library('reshape2')))
HourCasualRegistered <- melt(HourCasualRegistered[,c('Hour','casual','registered')], id.vars = 1)
colnames(HourCasualRegistered) <- c('Hour','RentType', 'Rents')

ggplot(data = HourCasualRegistered, mapping = aes(x = Hour, y = Rents)) +
  geom_bar(mapping = aes(fill = RentType), stat = "identity", position="dodge")+
  theme_minimal() +
  ggtitle("Registered Rentals are more popular during commute hours") + 
  theme(plot.title=element_text(size=18))


############################## modeling

######################data preparation for modeling
# import the data
url.hr <- "https://raw.githubusercontent.com/nitishghosal/Anomaly-Detection-Demand-Forecasting/master/hour.csv"
data.hr <- as_data_frame(read.csv(url.hr, stringsAsFactors = FALSE))

#View(data.hr)
# grouping the data

data.hr$hr[data.hr$hr == 0] <- "Night"
data.hr$hr[data.hr$hr == 1] <- "Night"
data.hr$hr[data.hr$hr == 2] <- "Night"
data.hr$hr[data.hr$hr == 3] <- "Night"
data.hr$hr[data.hr$hr == 4] <- "Night"
data.hr$hr[data.hr$hr == 5] <- "Night"
data.hr$hr[data.hr$hr == 6] <- "Morning"
data.hr$hr[data.hr$hr == 7] <- "Morning"
data.hr$hr[data.hr$hr == 8] <- "Morning"
data.hr$hr[data.hr$hr == 9] <- "Morning"
data.hr$hr[data.hr$hr == 10] <- "Morning"
data.hr$hr[data.hr$hr == 11] <- "Morning"
data.hr$hr[data.hr$hr == 12] <- "Afternoon"
data.hr$hr[data.hr$hr == 13] <- "Afternoon"
data.hr$hr[data.hr$hr == 14] <- "Afternoon"
data.hr$hr[data.hr$hr == 15] <- "Afternoon"
data.hr$hr[data.hr$hr == 16] <- "Afternoon"
data.hr$hr[data.hr$hr == 17] <- "Afternoon"
data.hr$hr[data.hr$hr == 18] <- "Evening"
data.hr$hr[data.hr$hr == 19] <- "Evening"
data.hr$hr[data.hr$hr == 20] <- "Evening"
data.hr$hr[data.hr$hr == 21] <- "Evening"
data.hr$hr[data.hr$hr == 22] <- "Evening"
data.hr$hr[data.hr$hr == 23] <- "Evening"
data.hr$hr<- as.factor(data.hr$hr)

data.hr$weekday[data.hr$weekday == 0] <- 0
data.hr$weekday[data.hr$weekday == 1] <- 1
data.hr$weekday[data.hr$weekday == 2] <- 1
data.hr$weekday[data.hr$weekday == 3] <- 1
data.hr$weekday[data.hr$weekday == 4] <- 1
data.hr$weekday[data.hr$weekday == 5] <- 1
data.hr$weekday[data.hr$weekday == 6] <- 0

data.hr$weekday <-as.integer(data.hr$weekday)

# convert categorical varables to factors
data.hr$season  <- factor(data.hr$season, labels = c("Spring", "Summer", "Fall", "Winter"))
data.hr$weathersit <- factor(data.hr$weathersit, labels = c("Good", "Normal", "Bad", "Very Bad"))


# for modeling purposes we are going to get rid of the following  variables
# instsant: which is the record index and it doesnt have any value on modeling
#dtday: as the date doesn't seem to have any impact given that we already have year,month,weekday, and hourly variables...
# temp, as we have atemp whihc is basically the same
# casual and registered becasue they are extremly highly correlated with cnt...
# month, as we have 12 levels and we could group them by season...becasue we have a seasons variable, we can just get rid of month

data.hr <- data.hr[,c(3:4,6:10,12:14,17)]
str(data.hr)


# splitting the data into train-test
set.seed(123)
index <- sample(nrow(data.hr),nrow(data.hr)*0.80)
bike.train <- data.hr[index,]
bike.test <- data.hr[-index,] 

#str(bike.train)
#str(bike.test)

##### Regression analysis

#Check Correlation

cor(data.hr[,-c(1:7)])
corrgram::corrgram(data.hr[,-c(1:7)], main="Correlation matrix of the continous variables")

#####glm - linear regression model
model <- lm(cnt ~ ., data = bike.train)
summary(model)  # why doesn't workinday variable take value?

### quickly variable selection.. but we can try lasso 
nullmodel<- lm(cnt~1, data=bike.train)
fullmodel<- lm(cnt~., data=bike.train)

model.step.s<- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='both')

summary(model.step.s)

##### Out of sample prediciton
pred.model0<- predict(model.step.s, newdata = bike.test)
MSPE <- mean((bike.test$cnt-pred.model0)^2)   
MSPE
### seems like a very high mspe...we can try other variable selection methods..although I don't think is going to improve a lot
# rememebr from the class that the  professor said that the model wasn't good for interpretation...so go to the end of this code to see another way of doing it...


##################################### decision tree

library(rpart)
library(rpart.plot)

######### pruning a tree ..using the boston data..
# we want to start with a big tree to make it smaller and not the opposite
largetree.bike <- rpart(formula = cnt ~ ., data = bike.train, cp = 0.001)  #this cp will make the tree very big
prp(largetree.bike, digits = 4, extra = 1)  # it is too big so we want to prune it

plotcp(largetree.bike) # plot of the cp's

printcp(largetree.bike) # last column is stand. dev
# so from the table and the plot decide what cp we want. Let's select n = 10 because even if it's not below the dotted line we gain a lot of interpetability

opt.cp <- largetree.bike$cptable[which(largetree.bike$cptable[,2]==10),1]  # we want to get the cp that has a size of 9...and we put 10 because a size of 10 has a 9 splits

prunedtree <-prune(largetree.bike, cp = opt.cp)
prp(prunedtree, digits = 4, extra = 1)

### prediciton
# in sample
bike.pred.tree <- predict(prunedtree) # this is in sample prediction
MSE <- mean((bike.train$cnt-bike.pred.tree)^2)

#testing
bike.tree.pred.test = predict(prunedtree, newdata=bike.test)
MSPE <- mean((bike.test$cnt-bike.tree.pred.test)^2)


## so the tree gets better values than the linear regression model



################################ bagging (from DMI lab notes)
library(ipred)

#Fit tree with bagging on Bike training data, and calculate MSE on testing sample.
bike.bag<- bagging(cnt~., data = bike.train, nbagg=100)
bike.bag

#Prediction on testing sample.

bike.bag.pred<- predict(bike.bag, newdata = bike.test)
mean((bike.test$cnt-bike.bag.pred)^2)  # we get pretty much the same value than with a single tree


#How many trees are good?

ntree<- c(1, 3, 5, seq(10, 200, 20))
MSE.test<- rep(0, length(ntree))
for(i in 1:length(ntree)){
  bike.bag1<- bagging(cnt~., data = bike.train, nbagg=ntree[i])
  bike.bag.pred1<- predict(bike.bag1, newdata = bike.test)
  MSE.test[i]<- mean((bike.test$cnt-bike.bag.pred1)^2)
}
plot(ntree, MSE.test, type = 'l', col=2, lwd=2)

# so based on this, we can use around 110 trees

bike.bag<- bagging(cnt~., data = bike.train, nbagg=110)
bike.bag

#Prediction on testing sample.

bike.bag.pred<- predict(bike.bag, newdata = bike.test)
mean((bike.test$cnt-bike.bag.pred)^2)
# very similar results to the CART


#### RANDOM FOREST


library(randomForest)
bike.rf<- randomForest(cnt~., data = bike.train, importance=TRUE)
bike.rf

#We specify importance=TRUE to see the variable imporatance.

bike.rf$importance

#The MSR is MSE of out-of-bag prediction (recall the OOB in bagging). The number of trees ntree=500 by default. The fitted randomForest actually saves all OOB errors for each ntree value from 1 to 500. We can make a plot to see how the OOB error changes with different ntree.
plot(bike.rf$mse, type='l', col=2, lwd=2, xlab = "ntree", ylab = "OOB.error")

#Prediction on the testing sample.

bike.rf.pred<- predict(bike.rf, bike.test)
mean((bike.test$cnt-bike.rf.pred)^2)   # improves a lot with random forest...around 20% improvement

#Variable Importance Plot
imp <- importance(bike.rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=16) +
  xlab("") + 
  ylab("Relative Importance") +
  theme(plot.title   = element_text(size=18),
        strip.text.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

######################## boosting

library(gbm)

bike.boost<- gbm(cnt~., data = bike.train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 5)
summary(bike.boost)

#### prediction on testing error....improves a little from random forest
bike.boost.pred.test<- predict(bike.boost, bike.test, n.trees = 10000)
mean((bike.test$cnt-bike.boost.pred.test)^2)

##We can investigate how the testing error changes with different number of trees.

ntree<- seq(100, 10000, 100)
predmat<- predict(bike.boost, newdata = bike.test, n.trees = ntree)
err<- apply((predmat-bike.test$cnt)^2, 2, mean)
plot(ntree, err, type = 'l', col=2, lwd=2, xlab = "n.trees", ylab = "Test MSE")
## so we can see that the error will very slighly decrease after n > 10,0000



################## GAM MODEL

str(bike.train)

library(mgcv)
### model the cateofrical variables normal and the continoious with s()

bike.gam <- gam(cnt ~season + yr+hr+holiday+weekday+workingday+ weathersit + s(atemp)+s(hum)+s(windspeed), data=bike.train)
summary(bike.gam)  # the s( ) terms have pretty high edf so we leave them as they are..



## in sample error
bike.gam.predict.train <- predict(bike.gam,bike.train)
bike.gam.mse.train <- mean((bike.gam.predict.train - bike.train[, "cnt"])^2)

# out of sample error
bike.gam.predict.test <- predict(bike.gam,bike.test) #Boston.gam built on training data
bike.gam.mse.test <- mean((bike.gam.predict.test - bike.test[, "cnt"])^2) ## out-of-sample

## conclusion...it improves the linear regression but doesn't improve random forest and boosting
## so we can see the improvement has come from modeling temperature, humidity and windspeed as smoothing variables!!

#########################################


############################### Neural network

############### neural network

### for neural network, we can only use numerical variables so we have to recode...
### what I am thinking we could do is to tranform the categorical variables into binary variables 0/1 to be able to introduce all of the variables in the model
## this is what I have done below...



data.hr$IsSummer <- ifelse(data.hr$season == "Summer", 
                           c(1), c(0)) 
data.hr$IsSpring <- ifelse(data.hr$season == "Spring", 
                           c(1), c(0)) 
data.hr$IsFall <- ifelse(data.hr$season == "Fall", 
                         c(1), c(0)) 
data.hr$IsWinter <- ifelse(data.hr$season == "Winter", 
                           c(1), c(0)) 


data.hr$IsMorning<- ifelse(data.hr$hr == "Morning", 
                           c(1), c(0)) 
data.hr$IsNight<- ifelse(data.hr$hr == "Night", 
                         c(1), c(0)) 
data.hr$IsEvening<- ifelse(data.hr$hr == "Evening", 
                           c(1), c(0)) 
data.hr$IsAfternoon<- ifelse(data.hr$hr == "Afternoon", 
                             c(1), c(0)) 

data.hr$IsGood <- ifelse(data.hr$weathersit == "Good", 
                         c(1), c(0)) 
data.hr$IsBad <- ifelse(data.hr$weathersit == "Bad", 
                        c(1), c(0))
data.hr$IsNormal<- ifelse(data.hr$weathersit == "Normal", 
                          c(1), c(0))
data.hr$IsVeryBAd <- ifelse(data.hr$weathersit == "Very Bad", 
                            c(1), c(0))
str(data.hr)
data.hr1 <- data.hr[,c(2,4:6,8:23)]
str(data.hr1)

set.seed(123)
index1 <- sample(nrow(data.hr),nrow(data.hr1)*0.80)
bike.train1 <- data.hr1[index1,]
bike.test1 <- data.hr1[-index1,] 

### case 1 scalling all the values...the numerical variables are already scale...so let's skip this part

data <- data.hr1
maxs <- apply(data.hr1, 2, max) 
mins <- apply(data.hr1, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

#install.packages("neuralnet")
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("cnt ~", paste(n[!n %in% "cnt"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3,3,3,3),linear.output=T)

plot(nn)

# predicitng using neural network-predicting the testing error

# in sample MSE
pr.nn1 <- compute(nn,train_[,c(1:7,9:20)])  # skipping cnt
pr.nn_1 <- pr.nn1$net.result*(max(data$cnt)-min(data$cnt))+min(data$cnt)
train.r <- (train_$cnt)*(max(data$cnt)-min(data$cnt))+min(data$cnt)
MSE.nn <- sum((train.r - pr.nn_1)^2)/nrow(train_)


# out of smaple MSPE
pr.nn <- compute(nn,test_[,c(1:7,9:20)])
pr.nn_ <- pr.nn$net.result*(max(data$cnt)-min(data$cnt))+min(data$cnt)

test.r <- (test_$cnt)*(max(data$cnt)-min(data$cnt))+min(data$cnt)
MSPE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
############################################################

## we get very good in sample but very bad out of sample!!! this might get caused by overfitting...so try to improve the out of sample error



############ trying linear regression with the new data configuration
View(data.hr1)

#glm
model3 <- lm(cnt ~ ., data = bike.train1)
summary(model3)  # why doesn't workinday variable take value?

### quickly variable selection...jsut for the proposal..
nullmodel3<- lm(cnt~1, data=bike.train1)
fullmodel3<- lm(cnt~., data=bike.train1)

model.step.s3<- step(nullmodel3, scope=list(lower=nullmodel3, upper=fullmodel3), direction='both')

summary(model.step.s3)

##### Out of sample prediciton
pred.model3<- predict(model.step.s3, newdata = bike.test1)
MSPE <- mean((bike.test1$cnt-pred.model3)^2)  

### very similar value (almost the same)...and the model has a pretty bad interpretability again (for instance, positive estimate for winter and negative for bad weather..how do we explain that). so I just think we can stick to the initial regression model


#######################LASSO###########################
# Variable selection for initial regression model

library(dplyr)
library(glmnet)

index <- sample(nrow(data.hr),nrow(data.hr)*0.80)

str(data.hr)

model.matrix()

dummy<- model.matrix(~ ., data = data.hr)


data.lasso<- data.frame(dummy[,-1])

X.train<- as.matrix(select(data.lasso,-cnt)[index,])
X.test<-  as.matrix(select(data.lasso,-cnt)[-index,])
Y.train<- data.lasso[index, "cnt"]
Y.test<- data.lasso[-index, "cnt"]

#Lasso
lasso.fit<- glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1)
plot(lasso.fit, xvar = "lambda")

lasso.fit$lambda

cv.lasso<- cv.glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1, nfolds = 10)
plot(cv.lasso)

cv.lasso$lambda.min

cv.lasso$lambda.1se

pred.lasso.min <- predict(lasso.fit, newx = X.test, s=cv.lasso$lambda.min)
pred.lasso.1se <- predict(lasso.fit, newx = X.test, s=cv.lasso$lambda.1se)
mspe.lasso<-mean((pred.lasso.1se-Y.test)^2)
mspe.lasso
#MSPE of 16357.64
#Model.Matrix is used to create dummy variables for the categorical variables
#I didn't standardize the variables for lasso because our coefficients would lose intrepretabilty when standardized
#In Data Mining 1 , Lab 5, Professor Li has followed the same approach

#################################################################################
##Multilayer Perceptron




































