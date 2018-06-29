#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#rpart
#Install and load necessary packages
install.packages("arules")
install.packages("arulesViz")
install.packages("party")
install.packages("rpart.plot")
install.packages("caret")
install.packages("pROC")
library("caret")
library("party")
library("rpart")
library("rpart.plot")
library("arules")
library("arulesViz")
library("pROC")
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

bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "frequency", breaks = 6)
bodyfat$Percent.body.fat.using.Brozek[] <- lapply(bodyfat$Percent.body.fat.using.Brozek, as.character)
head(bodyfat$Percent.body.fat.using.Brozek)

#Split data between training and test sets
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bodyfat[ind == 1, ]
test.data <- bodyfat[ind == 2, ]

#Create rpart formula
myFormula<-Percent.body.fat.using.Brozek~.

mod_rpart <- train(myFormula, train.data, method = "rpart",  tuneLength = 50, metric = "Accuracy",
                   trControl = trainControl(method = "repeatedcv", number = 10))
mod_rpart

modFitDecTree <- rpart(myFormula, train.data, method = "class", control = rpart.control(cp = mod_rpart$bestTune))

modFitDecTree
pred_rpart <- predict(mod_rpart, train.data)
confusionMatrix(pred_rpart, train.data$Percent.body.fat.using.Brozek)
#Visualize tree
rpart.plot(mod_rpart$finalModel, type = 4, extra = 101)

#Prune
#opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
#cp <- bodyfat_rpart$cptable[opt, "CP"]
#bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
#printcp(bodyfat_prune)
#summary(bodyfat_prune)
#rpart.plot(bodyfat_prune, type = 4, extra = 101)

#Predict on test data
testpred <- predict(mod_rpart, test.data)

actuals_preds <- data.frame(cbind(actuals=test.data$Percent.body.fat.using.Brozek, predicteds=testpred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy

mae <- function(actual,predicted) {mean(abs(actual - predicted))}
mae(actuals_preds$actuals, actuals_preds$predicteds)
mape <- function(actual,predicted) {mean(abs((predicted - actual))/actual)}
mape(actuals_preds$actuals, actuals_preds$predicteds)

confusionMatrix(testpred, test.data$Percent.body.fat.using.Brozek)

#Plot ROC
testpredprob <- predict(mod_rpart, test.data, type = "prob")
plot(roc(test.data$Percent.body.fat.using.Brozek, testpredprob[,2]))
