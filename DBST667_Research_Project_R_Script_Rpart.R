#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#rpart
#Install and load necessary packages
install.packages("arules")
install.packages("arulesViz")
install.packages("rpart.plot")
install.packages("caret")
install.packages("ROCR")
install.packages("klaR")
library("caret")
library("rpart")
library("rpart.plot")
library("arules")
library("arulesViz")
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

bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "frequency", breaks =3)
bodyfat$Percent.body.fat.using.Brozek
#Split data between training and test sets
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bodyfat[ind == 1, ]
test.data <- bodyfat[ind == 2, ]
#Create rpart formula
myFormula<-Percent.body.fat.using.Brozek~.
myFormula<-Percent.body.fat.using.Brozek~Height+Chest.circumference+Abdomen.circumference+Wrist.circumference
#rpart wo cross validation
modFitDecTree <- rpart(myFormula, train.data)
modFitDecTree
rpart.plot(modFitDecTree, type = 4, extra = 101)                                       
#rpart w/ cross-validation
my_cv_rpart <- train(myFormula, train.data, method = "rpart",  tuneLength = 100, metric = "Accuracy",
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5))

#Visualize tree
rpart.plot(my_cv_rpart$finalModel, type = 4, extra = 101)
#run predictions and matrix on training data
pred_rpart <- predict(my_cv_rpart, train.data)
confusionMatrix(pred_rpart, train.data$Percent.body.fat.using.Brozek)

#Predict on test data
testpred <- predict(my_cv_rpart, test.data)
confusionMatrix(testpred, test.data$Percent.body.fat.using.Brozek)

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


#Plot ROC
lvls = levels(bodyfat$Percent.body.fat.using.Brozek)

aucs = c()
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate',
     xlab='False Positive Rate',
     bty='n')
legend("bottomright", legend = c(lvls), col = c("red", "green", "blue"), lty=1:2, cex=0.8)
for (type.id in 1:3) {
  type = as.factor(train.data$Percent.body.fat.using.Brozek == lvls[type.id])
  
  my_rpart_prediction = predict(my_cv_rpart, test.data, type='prob')

  score = my_rpart_prediction[,type.id]

  actual.class = test.data$Percent.body.fat.using.Brozek == lvls[type.id]
  
  rpart_pred = prediction(score, actual.class)
  print(rpart_pred)
  rpart_perf = performance(rpart_pred, "tpr", "fpr")
  roc.x = unlist(rpart_perf@x.values)
  roc.y = unlist(rpart_perf@y.values)
  lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
  
  rpart_auc = performance(rpart_pred, "auc")
  rpart_auc = unlist(slot(rpart_auc, "y.values"))
  aucs[type.id] = rpart_auc
  
  #Find optimal cut off
#  opt.cut = function(rpart_perf, rpart_pred){
 #   cut.ind = mapply(FUN=function(x, y, p){
  #    d = (x - 0)^2 + (y-1)^2
   #   ind = which(d == min(d))
    #  c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
     #   cutoff = p[[ind]])
  #  }, rpart_perf@x.values, rpart_perf@y.values, rpart_pred@cutoffs)
  #}
  
  #opt_cut_val <- opt.cut(rpart_perf, rpart_pred)[3]
  #opt_cut_num <- match(opt_cut_val, rpart_pred@cutoffs[[1]][])
  
  print(lvls[type.id])
  print("Cutoffs:")
  print(rpart_pred@cutoffs[[1]])
  print("TP:")
  print(rpart_pred@tp[[1]])
  print("FP")
  print(rpart_pred@fp[[1]])
  print("TN")
  print(rpart_pred@tn[[1]])
  print("FN")
  print(rpart_pred@fn[[1]])
  print("-----")
  browser()
}

lines(x=c(0,1), c(0,1))
mean(aucs)



