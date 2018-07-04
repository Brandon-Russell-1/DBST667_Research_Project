#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#Multiple Linear Regression
#Install and Load necessary packages
install.packages("caret")
install.packages("ROCR")
install.packages("klaR")
library("caret")
library("ROCR")
library("klaR")
#Verify working directory
getwd()
#Load dataset
bodyfat <- read.csv("fat.csv", header = TRUE, sep = ",")
#View data to determine if needs any preprocessing
summary(bodyfat)
str(bodyfat)
#Verify no empty fields
apply(bodyfat, 2, function (bodyfat) sum(is.na(bodyfat)))
#Replace input error data with attribute mean 
bodyfat$Height[42]<-mean(bodyfat$Height, na.rm=TRUE)
#Remove row with multiple bad entries
bodyfat <- bodyfat[-182, ]
#Remove case number
bodyfat$Case.Number <- NULL
bodyfat$Density <- NULL
bodyfat$Fat.Free.Weight <- NULL
bodyfat$Percent.body.fat.using.Siri <- NULL
bodyfat$Adiposity.index <- NULL

#Split data between training and test sets
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bodyfat[ind == 1, ]
test.data <- bodyfat[ind == 2, ]

model <- lm(Percent.body.fat.using.Brozek~., data=train.data)
summary(model)

#Plot the model
par(mfrow = c(4,4))
plot(model)
plot(Percent.body.fat.using.Brozek~., data=train.data)

#remove the less significant feature
model2 = lm(Percent.body.fat.using.Brozek~Age+Weight+Neck.circumference
            +Chest.circumference+Abdomen.circumference
            +Hip.circumference+Thigh.circumference+Wrist.circumference, data=train.data)
summary(model2)

#Plot the model2
par(mfrow = c(4,4))
plot(model2)
plot(Percent.body.fat.using.Brozek~Age+Weight+Neck.circumference
     +Chest.circumference+Abdomen.circumference
     +Hip.circumference+Thigh.circumference+Wrist.circumference, data=train.data)

#remove the less significant feature
model3 = lm(Percent.body.fat.using.Brozek~Abdomen.circumference+Thigh.circumference+Wrist.circumference, data=train.data)
summary(model3)

#Plot the model3
par(mfrow = c(4,4))
plot(model3)
plot(Percent.body.fat.using.Brozek~Abdomen.circumference+Thigh.circumference+Wrist.circumference, data=train.data)

#Minimal adequate model test
stepmodel <- step(model, direction="backward")
summary(stepmodel)

#K-cross validation
set.seed(1234)
kcrossmodel <- train(Percent.body.fat.using.Brozek ~ Height + Chest.circumference + 
                       Abdomen.circumference + Wrist.circumference, train.data,
                     method = "lm", trControl = trainControl(
                       method = "cv", number = 10,
                       verboseIter = TRUE
                     )
)
summary(kcrossmodel)
kcrossmodel

#Predict on test data
testpred <- predict(kcrossmodel$finalModel, test.data, type = "response")
testpred
summary(testpred)


actuals_preds <- data.frame(cbind(actuals=test.data$Percent.body.fat.using.Brozek, predicteds=testpred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) # Return correlation accuracy
correlation_accuracy
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy # Return min max accuracy

# Calculate error
error <- actuals_preds$actuals - actuals_preds$predicteds
mae <- function(error) {mean(abs(error))}#Returns Mean Absolute Error
mape <- function(actual,predicted) {mean(abs((predicted - actual))/actual)}#returns Mean Absolute Percentage Error
rmse <- function(error){sqrt(mean(error^2))} #returns Root Mean Squared Error
#Run error check functions
rmse(error)
mae(error)
mape(actuals_preds$actuals, actuals_preds$predicteds)

#Scatter plot predictions
par(mfrow = c(1,1))
plot(test.data$Percent.body.fat.using.Brozek, testpred, xlab="Observed Values", ylab="Predicted Values")
abline(a=0, b=1)




