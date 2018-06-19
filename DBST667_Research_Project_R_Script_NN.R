#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#Neural Network
#Install and load necessary packages
#Load Library
library(neuralnet)
library("arules")
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

bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "frequency", breaks = 6)
head(bodyfat$Percent.body.fat.using.Brozek)
summary(bodyfat$Percent.body.fat.using.Brozek)


summary(bodyfat)
#Part 2ci - Create Seed and NeuralNet function
set.seed(12345)
ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bodyfat[ind == 1, ]
test.data <- bodyfat[ind == 2, ]

#Creative way of building NN formula
allVars <-colnames(train.data)
allVars
predictorVars <- allVars[!allVars%in%"Percent.body.fat.using.Brozek"]
predictorVars
predictorVars <- paste(predictorVars, collapse = "+")
predictorVars
form=as.formula(paste("Percent.body.fat.using.Brozek~",predictorVars,collapse = "+"))
form

#NN Model
nn<-neuralnet(formula = form, data = train.data, hidden=c(4,2), linear.output = T)


#Part 2 cii - Results Matrix
nn$result.matrix

#Part 2 ciii - Result Output
nn$net.result[[1]][1:10]

#Part 2 d - Plot the Neural Network

plot(nn)

#Part 2 ei - Eval the model

mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
head(mypredict,10)


# Part 2 eii -confusion matrix
table(mypredict, train.data$Percent.body.fat.using.Brozek)

#Part 2 fi - Report c,d,e w/ new parameters

#New NN Model
nn.new<-neuralnet(formula = form, data = VertCol, hidden=c(4,2), linear.output = T)
nn.new

#Part 2 fi - cii - Results Matrix
nn.new$result.matrix

#Part 2 fi - ciii - Result Output
nn.new$net.result[[1]][1:10]

#Part 2 fi - d - Plot the Neural Network

plot(nn.new)

#Part 2 fi - ei - Eval the model

mypredict<-compute(nn.new, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
head(mypredict,10)


# Part 2 fi - eii -confusion matrix
table(mypredict, VertCol$class)


