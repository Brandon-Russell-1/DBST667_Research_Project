#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#CTree
#Install and load necessary packages
install.packages("arules")
install.packages("arulesViz")
install.packages("party")
install.packages("rpart.plot")
library("party")
library("rpart")
library("rpart.plot")
library("arules", lib.loc="~/R/win-library/3.5")
library("arulesViz", lib.loc="~/R/win-library/3.5")
#Verify working directory
getwd()
#Load dataset
bodyfat <- read.csv("fat.csv", header = TRUE, sep = ",")
#View data to determine if needs any preprocessing
#View(bodyfat)
summary(bodyfat)
str(bodyfat)
#Verify no empty fields
apply(bodyfat, 2, function (bodyfat) sum(is.na(bodyfat)))
#Remove case number
bodyfat$Case.Number <- NULL
bodyfat$Density <- NULL
bodyfat$Fat.Free.Weight <- NULL
bodyfat$Percent.body.fat.using.Siri <- NULL
bodyfat$Adiposity.index <- NULL


#Part 2cii Split data between training and test sets
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bodyfat[ind == 1, ]
test.data <- bodyfat[ind == 2, ]

#Part 2di Run method on credit training data
myFormula<-Percent.body.fat.using.Brozek~.
bodyfat_ctree <- ctree(myFormula, data = train.data)
bodyfat_rpart <- rpart(myFormula, data = train.data)
#Part 2dii Print credit tree structure
print(bodyfat_ctree) 
print(bodyfat_rpart)
summary(bodyfat_rpart)
#Part 2de Visualize credit tree
plot(bodyfat_ctree, main = "Body Fat ctree")
plot(bodyfat_rpart, main = "Body Fat rpart")
text(bodyfat_rpart, use.n = T)
rpart.plot(bodyfat_rpart, compress = TRUE)

#Prune
opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
print(bodyfat_prune)
plot(bodyfat_prune)
text(bodyfat_prune, use.n=T)

DEXfat_pred <- predict(bodyfat_prune, newdata=test.data)
xlim <- range(bodyfat$Percent.body.fat.using.Brozek)
plot(DEXfat_pred ~ Percent.body.fat.using.Brozek, data=test.data, xlab="Observed", ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)

#Part 2fi Generate a confusion matrix
table(predict(bodyfat_ctree), train.data$Percent.body.fat.using.Brozek)
prop.table(table(predict(bodyfat_ctree), train.data$Percent.body.fat.using.Brozek))

#Part 2gi Evaluate the credit model on a test data
testPred <- predict(bodyfat_ctree, newdata = test.data)
table (testPred, test.data$Percent.body.fat.using.Brozek)
prop.table(table(testPred, test.data$Percent.body.fat.using.Brozek))


