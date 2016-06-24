setwd("C:/Users/DELL/Desktop/ISSMTECHASSIGNMENTS/Kaggle/US Presidental Poll/")
polling=read.csv("PollingData.csv")
str(polling)
# Rasmussen survey = polled R% - polled D% i.e., diff in % of voters who would vote for R and D during polls
# SurveyUSA is also similar to Rasmussen survey
# Diffcount = Polls with R winner - Polls with D winner
# propR = Polls with R winner / Total number of polls

table(polling$Year)
# Wee see that all 50 states were polled in 2004 and 2008, but 5 states were left out in 2012
# as pollsters were confident of the result in these 5 states, without conducting any polls

summary(polling)
# we notice that 46 and 71 missing values of Rasmussen and SurveyUSA values
# We can handle missing values by removing column or observation but thats not ideal here as it amounts to removing 50% data

# We observe that if propR is close to 1 then the SurveyUSA/Rasmussen is positive as Republican seem to winning
# similarly if propR is close to 0 then SurveyUSA/Rasmussen is negative as Democrats seem to winning
# thus we will use propR to fill NA's , this is called Multiple imputaion (Fill missing value based on non-missing value)
# also, if one survey reports the value to be very negative chances are that other survey value will also be negative

# We will use multiple imputaion by chained equations using mice package
# install.packages("mice")
library(mice)
# we create a new df but without the output/target variable and only having columns which are useful
simple_df=polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
set.seed(144)
imputed=complete(mice(simple_df))
summary(imputed)
# we see that 5 rounds of imputations are carried out and the missing values are completed/filled

# now we replace the imputed values into original polling dataframe
polling$Rasmussen=imputed$Rasmussen
polling$SurveyUSA=imputed$SurveyUSA
summary(polling)

# split the dataset, train on 2004 and 2008 and test on 2012 data
train=subset(polling,Year<2012)
test=subset(polling,Year==2012)

table(train$Republican)
# Wee see that Republicans have won more states, thus the baseline model will be always Republican winning the poll
# This baseline model will have accuracy of 53%
plot(polling$Rasmussen,polling$Republican)
plot(polling$SurveyUSA,polling$Republican)
# we see tha both the survey are good predictors
# thus we can caluclate a more smart baseline model which reads the survey data to predict the winner
# if the survey is positive then winner is republican else the winner is democrat
# we now use sign function on these surveys to predict outcome
smart_baseline=sign(train$Rasmussen)
table(smart_baseline)
table(train$Republican)
table(smart_baseline,train$Republican)
# we see that the accuracy of this smart baseline model is 42+52/100=94% which is far better that naive baseline model
# We will compare this model with the logistic regression model

cor(train[,-1])
# we see that there is multi-collinearity in the dataset
# Survey variables and propR are highly correlated to each other
# propR is most correlated with the target variable Republican, ,thus we choose this variable to predict the target
model1=glm(formula = Republican~PropR,family = "binomial",data = train)
summary(model1)
# Predict output in training set
predict1=predict(object = model1,type="response")
# we set thereshold of 0.5
table(train$Republican,predict1>0.5)
# the accurcy of the model 96% (better than the smart baseline model)
# we seek to improve this accuracy b by adding more variables which are highly correlated with target but
# low correlation with each other
# Rasmussen survey and diff count seem to those variables (SurveyUSA and diffcount are also good)
model2=glm(formula = Republican~SurveyUSA+DiffCount,family = "binomial",data = train)
summary(model2)
predict2=predict(object = model2,type="response")
table(train$Republican,predict2>0.5)

library(ROCR)
predictionTrain=prediction(predictions = predict2,labels = train$Republican)
performanceTrain=performance(prediction.obj = predictionTrain,"tpr","fpr")
plot(performanceTrain,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=0)
as.numeric(performance(prediction.obj = predictionTrain,"auc")@"y.values")


# The model accuracy has further improved to 97% and AUC is 99.7%, we see that there is some diasdvantages of the model
# Though the coeffiecnts are positive and AIC is less as expected the significance of variables for making the 
# prediction is very less

# We will compare which model performs better on the 2012 polls and choose appropriate model

# First we use smart baseline model on test set
table(test$Republican,sign(test$Rasmussen))
# accuracy of smart baseline model is 29/45=64.4%

# Next, we use model 1 on test set
predictTest1=predict(object = model1,newdata = test,type = "response")
table(test$Republican,predictTest1>0.5)
# Accuracy of this model is 97.9778%

# Next, we use model 2 on test set
predictTest2=predict(object = model2,newdata = test,type = "response")
table(test$Republican,predictTest2>0.5)
# Accuracy of this model is 97.9778%



# We see the observation where our model failed, where we predicted Republican would win but a Democrat won
subset(test,predictTest2>0.5 & Republican==0)
# This was the state of florida which is swing state as it frequently changes its parrty loyalty !!






