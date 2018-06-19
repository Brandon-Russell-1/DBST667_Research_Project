#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#Support Vector Machine
#Load necessary packages
library(e1071)
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
bodyfat$Height <- NULL
bodyfat$Knee.circumference <- NULL
bodyfat$Ankle.circumference <- NULL
bodyfat$Extended.biceps.circumference <- NULL
bodyfat$Forearm.circumference <- NULL
bodyfat$Wrist.circumference <- NULL
bodyfat$Age <- NULL

bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "frequency", breaks = 6)
head(bodyfat$Percent.body.fat.using.Brozek)

summary(bodyfat)
str(bodyfat)

plot(bodyfat)

plot(bodyfat$Abdomen.circumference, bodyfat$Weight, col=bodyfat$Percent.body.fat.using.Brozek)
plot(bodyfat$Abdomen.circumference, bodyfat$Hip.circumference, col=bodyfat$Percent.body.fat.using.Brozek)

#set.seed(1234)
#ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7, 0.3))
#bodyfat_train <- bodyfat[ind == 1, ]
#bodyfat_test <- bodyfat[ind == 2, ]

s <-sample(150,100)
s
col<-c("Thigh.circumference","Abdomen.circumference","Percent.body.fat.using.Brozek")
bodyfat_train <- bodyfat[s, col]
bodyfat_test <- bodyfat[-s, col]

svmfit <- svm(Percent.body.fat.using.Brozek~., data = bodyfat_train, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

tuned <- tune(svm, Percent.body.fat.using.Brozek~., data = bodyfat_train, kernel = "linear", ranges = list(cost=c(0.001, 0.01,.1, 10, 100)))
summary(tuned)

p <- predict(svmfit, bodyfat_test[, col], type="class")
plot(p)
table(p, bodyfat_test[,3])
mean(p == bodyfat_test[,3])
