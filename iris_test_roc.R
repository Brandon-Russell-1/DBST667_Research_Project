library("MLmetrics")
library("caret")
library("party")
library("rpart")
library("rpart.plot")
library("arules")
library("arulesViz")
library("pROC")
library("ROCR")
library("klaR")

data(iris)

lvls = levels(iris$Species)
lvls
testidx = which(1:length(iris[, 1]) %% 5 == 0) 
iris.train = iris[testidx, ]
iris.test = iris[-testidx, ]

str(iris.train)
aucs = c()
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate',
     xlab='False Positive Rate',
     bty='n')

for (type.id in 1:3) {
  type = as.factor(iris.train$Species == lvls[type.id])
  iris.train$Species
  type
  nbmodel = NaiveBayes(type ~ ., data=iris.train[, -5])
  nbmodel
  nbprediction = predict(nbmodel, iris.test[,-5], type='raw')
  iris.test[,-5]
  nbprediction
  score = nbprediction$posterior[, 'TRUE']
  score
  actual.class = iris.test$Species == lvls[type.id]
  actual.class
  pred = prediction(score, actual.class)
  nbperf = performance(pred, "tpr", "fpr")
  
  roc.x = unlist(nbperf@x.values)
  roc.y = unlist(nbperf@y.values)
  lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
  
  nbauc = performance(pred, "auc")
  nbauc = unlist(slot(nbauc, "y.values"))
  aucs[type.id] = nbauc
}

lines(x=c(0,1), c(0,1))

mean(aucs)