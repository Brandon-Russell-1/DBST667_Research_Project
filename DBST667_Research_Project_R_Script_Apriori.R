#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#Apriori Method
#Load necessary packages
#install.packages("arules")
#install.packages("arulesViz")
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
apply(bodyfat, 2, function (credit) sum(is.na(bodyfat)))
#Remove case number
bodyfat$Case.Number <- NULL
bodyfat$Density <- NULL
bodyfat$Fat.Free.Weight <- NULL
bodyfat$Percent.body.fat.using.Siri <- NULL
bodyfat$Adiposity.index <- NULL
bodyfat$Height <- NULL
#View(bodyfat)

#Perform discretization on int/num attributes to perform the Apriori method, and check work
bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "cluster")
head(bodyfat$Percent.body.fat.using.Brozek)

#bodyfat$Percent.body.fat.using.Siri <- discretize(bodyfat$Percent.body.fat.using.Siri, "cluster", breaks = 4)
#head(bodyfat$Percent.body.fat.using.Siri)

#bodyfat$Density <- discretize(bodyfat$Density, "cluster" )
#head(bodyfat$Density)

bodyfat$Age <- discretize(bodyfat$Age, "cluster", breaks = 4)
head(bodyfat$Age)

bodyfat$Weight <- discretize(bodyfat$Weight, "cluster", breaks = 4 )
head(bodyfat$Weight)

#bodyfat$Height <- discretize(bodyfat$Height, "cluster", breaks = 4 )
#head(bodyfat$Height)

#bodyfat$Adiposity.index <- discretize(bodyfat$Adiposity.index, "cluster", breaks = 4)
#head(bodyfat$Adiposity.index)

#bodyfat$Fat.Free.Weight <- discretize(bodyfat$Fat.Free.Weight, "cluster" )
#head(bodyfat$Fat.Free.Weight)

bodyfat$Neck.circumference <- discretize(bodyfat$Neck.circumference, "cluster", breaks = 4 )
head(bodyfat$Neck.circumference)

bodyfat$Chest.circumference <- discretize(bodyfat$Chest.circumference, "cluster", breaks = 4)
head(bodyfat$Chest.circumference)

bodyfat$Abdomen.circumference <- discretize(bodyfat$Abdomen.circumference, "cluster", breaks = 4)
head(bodyfat$Abdomen.circumference)

bodyfat$Hip.circumference <- discretize(bodyfat$Hip.circumference, "cluster", breaks = 4 )
head(bodyfat$Hip.circumference)

bodyfat$Thigh.circumference <- discretize(bodyfat$Thigh.circumference, "cluster", breaks = 4 )
head(bodyfat$Thigh.circumference)

bodyfat$Knee.circumference <- discretize(bodyfat$Knee.circumference, "cluster", breaks = 4 )
head(bodyfat$Knee.circumference)

bodyfat$Ankle.circumference <- discretize(bodyfat$Ankle.circumference, "cluster", breaks = 4 )
head(bodyfat$Ankle.circumference)

bodyfat$Extended.biceps.circumference <- discretize(bodyfat$Extended.biceps.circumference, "cluster", breaks = 4 )
head(bodyfat$Extended.biceps.circumference.circumference)

bodyfat$Forearm.circumference <- discretize(bodyfat$Forearm.circumference, "cluster", breaks = 4 )
head(bodyfat$Forearm.circumference)

bodyfat$Wrist.circumference <- discretize(bodyfat$Wrist.circumference, "cluster", breaks = 4)
head(bodyfat$Wrist.circumference)

#Verify completion
summary(bodyfat)
str(bodyfat)
head(bodyfat)

#Run Apriori method with defaults
rules<-apriori(bodyfat)
rules
inspect(rules[1:10])
#Change default values
rules<-apriori(bodyfat, parameter = list(minlen=2, supp=0.1, conf=0.9))
inspect(rules[1:10])

summary(bodyfat$Percent.body.fat.using.Siri)
rules<-apriori(bodyfat, appearance = list(rhs=c("Percent.body.fat.using.Siri=[0,12.4)", "Percent.body.fat.using.Siri=[12.4,20)", "Percent.body.fat.using.Siri=[20,27.7)", "Percent.body.fat.using.Siri=[27.7,47.5]"), default="lhs"))
inspect(rules[1:10])

#Find redundant rules
rules.sorted<-sort(rules, by="lift")
inspect(rules.sorted[1:10])
redundant <- is.redundant(rules.sorted)
redundant
which(redundant)
rules.pruned<-rules.sorted[!redundant]
rules.pruned
inspect(rules.pruned)
rules.dataframe <- as(rules.pruned, "data.frame")
write.csv(rules.dataframe, file = "rules_data.csv")
#Provide a plot of the support, confidence, and support of pruned rules
plot(rules.pruned)
