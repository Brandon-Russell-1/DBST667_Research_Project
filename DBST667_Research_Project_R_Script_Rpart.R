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
#Load dataset...start here when reloading data for third, fourth, and fifth model
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

#First Model Discretization
bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "frequency", breaks =6)
#Perform next line for third model only
bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "frequency", breaks =8)
#Perform next line for fourth model only
bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "frequency", breaks =4)
#Perform next line for fifth model only
bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "frequency", breaks =3)
bodyfat$Percent.body.fat.using.Brozek
#Split data between training and test sets
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bodyfat[ind == 1, ]
test.data <- bodyfat[ind == 2, ]
#From here you can run models one and two, or skip to three, four, or five
#Create rpart formula
#First Model 
myFormula<-Percent.body.fat.using.Brozek~.
#rpart w/o cross validation
modFitDecTree <- rpart(myFormula, train.data)
modFitDecTree
#Visualize tree
rpart.plot(modFitDecTree, type = 4, extra = 101)                                       
#run predictions and matrix on training data
pred_rpart <- predict(modFitDecTree, train.data, type = "class")
confusionMatrix(pred_rpart, train.data$Percent.body.fat.using.Brozek)
#run predictions and matrix on test data
testpred <- predict(modFitDecTree, test.data, type = "class")
confusionMatrix(testpred, test.data$Percent.body.fat.using.Brozek)

#Second Model
myFormula<-Percent.body.fat.using.Brozek~Height+Chest.circumference+Abdomen.circumference+Wrist.circumference
#rpart w/ cross-validation #On second
my_cv_rpart <- train(myFormula, train.data, method = "rpart",  tuneLength = 50, metric = "Accuracy",
                     trControl = trainControl(method = "repeatedcv", number = 10))
#Visualize tree
rpart.plot(my_cv_rpart$finalModel, type = 4, extra = 101)
pred_rpart <- predict(my_cv_rpart, train.data)
#Run on both
confusionMatrix(pred_rpart, train.data$Percent.body.fat.using.Brozek)
#Cross-validated model predict
testpred <- predict(my_cv_rpart, test.data)
#Run on both
confusionMatrix(testpred, test.data$Percent.body.fat.using.Brozek)

#Perform Data reload at top with frequency set to 8, then return here.
#Third Model
myFormula<-Percent.body.fat.using.Brozek~Height+Chest.circumference+Abdomen.circumference+Wrist.circumference
#rpart w/ cross-validation #On second
my_cv_rpart <- train(myFormula, train.data, method = "rpart",  tuneLength = 100, metric = "Accuracy",
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5))
#Visualize tree
rpart.plot(my_cv_rpart$finalModel, type = 4, extra = 101)
pred_rpart <- predict(my_cv_rpart, train.data)
#Run on both
confusionMatrix(pred_rpart, train.data$Percent.body.fat.using.Brozek)
#Cross-validated model predict
testpred <- predict(my_cv_rpart, test.data)
#Run on both
confusionMatrix(testpred, test.data$Percent.body.fat.using.Brozek)

#Fourth & Fifth Model
myFormula<-Percent.body.fat.using.Brozek~.
#rpart w/ cross-validation #On second
my_cv_rpart <- train(myFormula, train.data, method = "rpart",  tuneLength = 100, metric = "Accuracy",
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5))
#Visualize tree
rpart.plot(my_cv_rpart$finalModel, type = 4, extra = 101)
pred_rpart <- predict(my_cv_rpart, train.data)
#Run on both
confusionMatrix(pred_rpart, train.data$Percent.body.fat.using.Brozek)
#Cross-validated model predict
testpred <- predict(my_cv_rpart, test.data)
#Run on both
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
#Reference Line
lines(x=c(0,1), c(0,1))
#Mean Area Under Curve
mean(aucs)



