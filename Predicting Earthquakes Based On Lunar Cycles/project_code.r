# Import packages
library(plyr)
library(gmodels)
library(caret)
library(rpart)
library(party)
library(randomForest)
library(ggplot2)
library(sandwich)
library(lmtest)
library(pscl)

#Import dataset
northcali.df <- read.csv("~/Projects/DataAnalytics2016/Project/NorthCaliforniaFaultEarthquakes.txt", header=TRUE, sep="|")
moonphase.df <- read.csv("~/Projects/DataAnalytics2016/Project/MoonPhasePercent.csv", header=TRUE, sep=",")

#Transformations
moonphase.df$Day <- as.Date(moonphase.df$Day, "%m/%d/%y")
northcali.df$Day <- substr(northcali.df$Time, 1, 10)
northcali.df$Day <- as.Date(northcali.df$Day)
northcali.df <- subset(northcali.df, Day > as.Date("2006-12-31") )


#EDA
boxplot(northcali.df$Magnitude, xlab="Magnitude Boxplot", ylab="Magnitude")
boxplot(moonphase.df$Percent, xlab="Percent Illumination Boxplot", ylab="Percent")

#More transformations and feature extraction
northcali.df$Year <- as.factor(substr(northcali.df$Time,1,4))
northcali.df$Month <- as.factor(substr(northcali.df$Time,6,7))
northcali.df$Days <- as.factor(substr(northcali.df$Time,9,10))


#More EDA
northcali.year_agg <- count(northcali.df, "Year")
plot(northcali.year_agg$Year, northcali.year_agg$freq)

northcali.month_agg <- count(northcali.df, "Month")
plot(northcali.month_agg$Month, northcali.month_agg$freq)

northcali.days_agg <- count(northcali.df, "Days")
plot(northcali.days_agg$Days, northcali.days_agg$freq)


# Dataset transformation
northcali.date_agg <- count(northcali.df, "Day")

# Filtering out rows
northcali.date_agg <- subset(northcali.date_agg, Day > as.Date("2006-12-31") )

#Merge two datasets Earthquake and Moon Illumination
northcali.date_agg <- join(northcali.date_agg, moonphase.df, by='Day', type='right')

#More EDA / Plots frequency distribution
ggplot(northcali.date_agg, aes(x=Year, y=freq)) + geom_boxplot()
ggplot(northcali.date_agg, aes(x=Percent)) + geom_density()
ggplot(northcali.date_agg, aes(x=Month, y=freq)) + geom_boxplot()
ggplot(northcali.date_agg, aes(x=Days, y=freq)) + geom_boxplot()

# New feature for occurance of earthquake
northcali.date_agg[is.na(northcali.date_agg)] <- 0
northcali.date_agg$Class <- as.factor(northcali.date_agg$freq > 0)

# Year, Month, Day as factor
northcali.date_agg$Year <- as.factor(format(northcali.date_agg$Day,'%Y'))
northcali.date_agg$Month <- as.factor(format(northcali.date_agg$Day,'%m'))
northcali.date_agg$Days <- as.factor(format(northcali.date_agg$Day,'%d'))


# Poisson regression model
northcali.poiss_model <- glm(freq~1+Percent, data=northcali.date_agg, family=poisson(link=log))
predictions=data.frame(northcali.date_agg,pred=northcali.poiss_model$fitted)
predictions
summary(northcali.poiss_model)

# Poisson regression model with variation in input variables
northcali.poiss_model2 <- glm(freq~Days+Year+Month, data=northcali.date_agg, family=poisson(link=log))
predictions=data.frame(northcali.date_agg,pred=northcali.poiss_model2$fitted)
predictions
summary(northcali.poiss_model2)

# Dataset partitioning for training and testing
set.seed(42)
train <- createDataPartition(northcali.date_agg$freq, p=0.90, list=FALSE)
northcali.date_agg.training <- northcali.date_agg[ train, ]
northcali.date_agg.testing <- northcali.date_agg[ -train, ]


# Rpart model for all input variables
northcali.model1 <- rpart(Class~Year+Month+Days+Percent, data=northcali.date_agg.training, method = "class")
plotcp(northcali.model1) 
printcp(northcali.model1)
plot(northcali.model1)
text(northcali.model1)
northcali.pred1 <- predict(northcali.model1, northcali.date_agg.testing)
northcali.pred1$Class <- as.factor(northcali.pred1[,1] < northcali.pred1[,2])
mean(northcali.pred1$Class == northcali.date_agg.testing$Class)
northcali.pred1$Class

# Rpart for just Year as input variable
northcali.model2 <- rpart(Class~Year, data=northcali.date_agg.training, method = "class")
plotcp(northcali.model2) 
printcp(northcali.model2)
northcali.pred2 <- predict(northcali.model2, northcali.date_agg.testing)
northcali.pred2$Class <- as.factor(northcali.pred2[,1] < northcali.pred2[,2])
mean(northcali.pred2$Class == northcali.date_agg.testing$Class)


# Random Forest for all input variables
northcali.rf_model <- randomForest(Class~Year+Month+Days+Percent, data=northcali.date_agg.training)
plot(northcali.rf_model)
print(northcali.rf_model)
varImpPlot(northcali.rf_model)
northcali.rf_pred <- predict(northcali.rf_model, northcali.date_agg.testing)
northcali.pred1
mean(northcali.rf_pred == northcali.date_agg.testing$Class)

# Final Random Forest with just two input variables Year and Day. 
# Best model based on prediction accuracy
northcali.rf_model1 <- randomForest(Class~Year+Days, data=northcali.date_agg.training)
plot(northcali.rf_model1)
varImpPlot(northcali.rf_model1)
northcali.rf_pred1 <- predict(northcali.rf_model1, northcali.date_agg.testing)
mean(northcali.rf_pred1 == northcali.date_agg.testing$Class)
print(northcali.rf_model1)
