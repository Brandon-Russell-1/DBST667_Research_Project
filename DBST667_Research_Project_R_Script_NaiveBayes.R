#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#Naive Bayes
#Install and load necessary packages
install.packages("e1071")
install.packages("arules")
install.packages("caret")
install.packages("naivebayes")
#Load Library files
library("arules")
library("e1071")
library("caret")
library("klaR")
library("naivebayes")
#Verify working directory
getwd()
#Load dataset - Start here if reloading for fourth model
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

#Discretize dependent variable
bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "frequency", breaks = 3)
#Discretize independent variable - For fourth model do not perform discretization on independent variables
bodyfat$Age <- discretize(bodyfat$Age, "frequency", breaks = 3)
bodyfat$Weight <- discretize(bodyfat$Weight, "frequency", breaks = 3 )
bodyfat$Height <- discretize(bodyfat$Height, "frequency", breaks = 3 )
bodyfat$Neck.circumference <- discretize(bodyfat$Neck.circumference, "frequency", breaks = 3 )
bodyfat$Chest.circumference <- discretize(bodyfat$Chest.circumference, "frequency", breaks = 3)
bodyfat$Abdomen.circumference <- discretize(bodyfat$Abdomen.circumference, "frequency", breaks = 3)
bodyfat$Hip.circumference <- discretize(bodyfat$Hip.circumference, "frequency", breaks = 3 )
bodyfat$Thigh.circumference <- discretize(bodyfat$Thigh.circumference, "frequency", breaks = 3 )
bodyfat$Knee.circumference <- discretize(bodyfat$Knee.circumference, "frequency", breaks = 3 )
bodyfat$Ankle.circumference <- discretize(bodyfat$Ankle.circumference, "frequency", breaks = 3 )
bodyfat$Extended.biceps.circumference <- discretize(bodyfat$Extended.biceps.circumference, "frequency", breaks = 3 )
bodyfat$Forearm.circumference <- discretize(bodyfat$Forearm.circumference, "frequency", breaks = 3 )
bodyfat$Wrist.circumference <- discretize(bodyfat$Wrist.circumference, "frequency", breaks = 3)

#Split data between training and test sets
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bodyfat[ind == 1, ]
test.data <- bodyfat[ind == 2, ]
#From here you can run models one and two or skip to three and four
myFormula<-Percent.body.fat.using.Brozek~.
#First Model
model<-naiveBayes(myFormula, train.data)
#output the model
model
summary(model)
str(model)

#confusion matrix for the training set; need to round the estimated values
trainpred <- predict(model, train.data)
summary(trainpred)
confusionMatrix(trainpred, train.data$Percent.body.fat.using.Brozek)

#confusion matrix for the test data
testpred <- predict(model, test.data)
summary(testpred)
confusionMatrix(testpred, test.data$Percent.body.fat.using.Brozek)




#Second model w/ cross-validation
my_cv_nb <- train(myFormula, train.data, method = "naive_bayes",  tuneLength = 100, metric = "Accuracy",
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5))

my_cv_nb
my_cv_nb$finalModel
summary(my_cv_nb)
#confusion matrix for the training set; need to round the estimated values
trainpred <- predict(my_cv_nb, train.data)
summary(trainpred)
confusionMatrix(trainpred, train.data$Percent.body.fat.using.Brozek)

#confusion matrix for the test data
testpred <- predict(my_cv_nb, test.data)
summary(testpred)
confusionMatrix(testpred, test.data$Percent.body.fat.using.Brozek)


#Third model & Fourth Model - For fourth model go back and only discretize dependent variable
#Modified formula
myFormula<-Percent.body.fat.using.Brozek~Height+Chest.circumference+Abdomen.circumference+Wrist.circumference
#Run the model
my_cv_nb <- train(myFormula, train.data, method = "naive_bayes",  tuneLength = 100, metric = "Accuracy",
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5))

my_cv_nb
my_cv_nb$finalModel
summary(my_cv_nb)
#confusion matrix for the training set; need to round the estimated values
trainpred <- predict(my_cv_nb, train.data)
summary(trainpred)
confusionMatrix(trainpred, train.data$Percent.body.fat.using.Brozek)

#confusion matrix for the test data
testpred <- predict(my_cv_nb, test.data)
summary(testpred)
confusionMatrix(testpred, test.data$Percent.body.fat.using.Brozek)






#Perform accuracy, error, and ROC checks on final model
# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=test.data$Percent.body.fat.using.Brozek, predicteds=testpred))  
# Return correlation accuracy
correlation_accuracy <- cor(actuals_preds) 
correlation_accuracy
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# Return min max accuracy
min_max_accuracy 

# Calculate error
error <- actuals_preds$actuals - actuals_preds$predicteds
mae <- function(error) {mean(abs(error))}#Returns Mean Absolute Error
mape <- function(actual,predicted) {mean(abs((predicted - actual))/actual)}#returns Mean Absolute Percentage Error
rmse <- function(error){sqrt(mean(error^2))} #returns Root Mean Squared Error
#Run error check functions
rmse(error)
mae(error)
mape(actuals_preds$actuals, actuals_preds$predicteds)


#Plot ROC after final model is selected
lvls = levels(bodyfat$Percent.body.fat.using.Brozek)
#Setup plot field
aucs = c()
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate',
     xlab='False Positive Rate',
     bty='n')
legend("bottomright", legend = c(lvls), col = c("red", "green", "blue"), lty=1:2, cex=0.8)
#Run loop to build ROC for each bin
for (type.id in 1:3) {
  type = as.factor(train.data$Percent.body.fat.using.Brozek == lvls[type.id])
  
  my_nb_prediction = predict(my_cv_nb, test.data, type='prob')
  
  score = my_nb_prediction[,type.id]
  actual.class = test.data$Percent.body.fat.using.Brozek == lvls[type.id]
  
  nb_pred = prediction(score, actual.class)
  print(nb_pred)
  nb_perf = performance(nb_pred, "tpr", "fpr")
  roc.x = unlist(nb_perf@x.values)
  roc.y = unlist(nb_perf@y.values)
  lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
  
  nb_auc = performance(nb_pred, "auc")
  nb_auc = unlist(slot(nb_auc, "y.values"))
  aucs[type.id] = nb_auc
  
  #Find optimal cut off
  #  opt.cut = function(nb_perf, nb_pred){
  #   cut.ind = mapply(FUN=function(x, y, p){
  #    d = (x - 0)^2 + (y-1)^2
  #   ind = which(d == min(d))
  #  c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
  #   cutoff = p[[ind]])
  #  }, nb_perf@x.values, nb_perf@y.values, nb_pred@cutoffs)
  #}
  
  #opt_cut_val <- opt.cut(nb_perf, nb_pred)[3]
  #opt_cut_num <- match(opt_cut_val, nb_pred@cutoffs[[1]][])
  
  print(lvls[type.id])
  print("Cutoffs:")
  print(nb_pred@cutoffs[[1]])
  print("TP:")
  print(nb_pred@tp[[1]])
  print("FP")
  print(nb_pred@fp[[1]])
  print("TN")
  print(nb_pred@tn[[1]])
  print("FN")
  print(nb_pred@fn[[1]])
  print("-----")
  browser()
}

#Reference Line
lines(x=c(0,1), c(0,1))
#Mean Area Under Curve
mean(aucs)

