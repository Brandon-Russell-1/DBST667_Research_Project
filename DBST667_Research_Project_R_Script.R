#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#Load necessary packages
install.packages("arules")
install.packages("arulesViz")
library("arules", lib.loc="~/R/win-library/3.5")
library("arulesViz", lib.loc="~/R/win-library/3.5")
#Verify working directory
getwd()
#Load dataset
bodyfat <- read.csv("fat.csv", header = TRUE, sep = ",")
#View data to determine if needs any preprocessing
View(bodyfat)
summary(bodyfat)
str(bodyfat)
#Verify no empty fields
apply(bodyfat, 2, function (credit) sum(is.na(bodyfat)))
#Remove case number
bodyfat$Case.Number <- NULL
View(bodyfat)
#Perform discretization on int/num attributes to perform the Apriori method, and check work
bodyfat$Percent.body.fat.using.Brozek <- discretize(bodyfat$Percent.body.fat.using.Brozek, "cluster", breaks = 6)
head(bodyfat$Percent.body.fat.using.Brozek)

bodyfat$Percent.body.fat.using.Siri <- discretize(bodyfat$Percent.body.fat.using.Siri, "cluster", breaks = 6)
head(bodyfat$Percent.body.fat.using.Siri)

#Run Apriori method with defaults
rules<-apriori(credit)
rules
inspect(rules[1:10])
#Part2di
#Change default values twice and discuss
rules<-apriori(credit, parameter = list(minlen=2, supp=0.2, conf=0.9))
inspect(rules[1:10])
rules<-apriori(credit, parameter = list(minlen=2, supp=0.2, conf=0.9), appearance = list(rhs=c("A5=p", "A9=t"), default="lhs"))
inspect(rules[1:10])
#Part2e
#Include only transactions with rhs=class[+/-]
rules<-apriori(credit, appearance = list(rhs=c("class=+", "class=-"), default="lhs"))
inspect(rules[1:10])
summary(credit$A7)
summary(credit$A9)
#Part2fii
#Find redundant rules
rules.sorted<-sort(rules, by="lift")
inspect(rules.sorted)
#subset.matrix<-is.subset(rules.sorted, rules.sorted)
#subset.matrix[lower.tri(subset.matrix, diag=T)]<-NA #This gives error. Fix is below
#subset.matrix
#redundant<-colSums(subset.matrix, na.rm=T)>=1 
#which(redundant)
#Remove the redundant rules
#rm(subset.matrix)
#rm(redundant)
redundant <- is.redundant(rules.sorted)
redundant
which(redundant)
#Part2fiii
rules.pruned<-rules.sorted[!redundant]
inspect(rules.pruned)
#Part2g
#Provide a plot of the support, confidence, and support of pruned rules
plot(rules.pruned)
