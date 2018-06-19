#install the e1071 package.  Only once.
install.packages("e1071")

#load the arules and e1071 library into memory
#need to do this each time you start the new R session
library("arules")
library("e1071")

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
head(bodyfat$Percent.body.fat.using.Siri)

bodyfat$Age <- discretize(bodyfat$Age, "frequency", breaks = 6)
head(bodyfat$Age)

bodyfat$Weight <- discretize(bodyfat$Weight, "frequency", breaks = 6 )
head(bodyfat$Weight)

bodyfat$Height <- discretize(bodyfat$Height, "frequency", breaks = 6 )
head(bodyfat$Height)

bodyfat$Neck.circumference <- discretize(bodyfat$Neck.circumference, "frequency", breaks = 6 )
head(bodyfat$Neck.circumference)

bodyfat$Chest.circumference <- discretize(bodyfat$Chest.circumference, "frequency", breaks = 6)
head(bodyfat$Chest.circumference)

bodyfat$Abdomen.circumference <- discretize(bodyfat$Abdomen.circumference, "frequency", breaks = 6)
head(bodyfat$Abdomen.circumference)

bodyfat$Hip.circumference <- discretize(bodyfat$Hip.circumference, "frequency", breaks = 6 )
head(bodyfat$Hip.circumference)

bodyfat$Thigh.circumference <- discretize(bodyfat$Thigh.circumference, "frequency", breaks = 6 )
head(bodyfat$Thigh.circumference)

bodyfat$Knee.circumference <- discretize(bodyfat$Knee.circumference, "frequency", breaks = 6 )
head(bodyfat$Knee.circumference)

bodyfat$Ankle.circumference <- discretize(bodyfat$Ankle.circumference, "frequency", breaks = 6 )
head(bodyfat$Ankle.circumference)

bodyfat$Extended.biceps.circumference <- discretize(bodyfat$Extended.biceps.circumference, "frequency", breaks = 6 )
head(bodyfat$Extended.biceps.circumference.circumference)

bodyfat$Forearm.circumference <- discretize(bodyfat$Forearm.circumference, "frequency", breaks = 6 )
head(bodyfat$Forearm.circumference)

bodyfat$Wrist.circumference <- discretize(bodyfat$Wrist.circumference, "frequency", breaks = 6)
head(bodyfat$Wrist.circumference)


#make sure that the result is reproducible
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bodyfat[ind == 1, ]
test.data <- bodyfat[ind == 2, ]

#build the model and store in a variable model
model<-naiveBayes(Percent.body.fat.using.Brozek~., train.data)
#output the model
model
summary(model)
str(model)
#confusion matrix for the training set; need to round the estimated values
table(predict(model, train.data), train.data$Percent.body.fat.using.Brozek)
prop.table(table(predict(model, train.data), train.data$Percent.body.fat.using.Brozek))
#confusion matrix for the test data
table(predict(model, test.data), test.data$Percent.body.fat.using.Brozek)
prop.table(table(predict(model, test.data), test.data$Percent.body.fat.using.Brozek))
#mosaic plot
mosaicplot(table(predict(model, test.data), test.data$Percent.body.fat.using.Brozek), shade=TRUE, main="Predicted vs. Actual Siri")
