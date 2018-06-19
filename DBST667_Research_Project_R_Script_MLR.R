#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#Multiple Linear Regression
#Load necessary packages
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

#Build distributions for Figure 1
library(ggplot2)
library(tidyr)

bodyfat %>% gather() %>% head()

ggplot(gather(bodyfat), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#Build table 1 for paper
sapply(bodyfat, sd, na.rm = TRUE)
tmp <- do.call(data.frame, 
               list(mean = apply(bodyfat, 2, mean),
                    sd = apply(bodyfat, 2, sd),
                    median = apply(bodyfat, 2, median),
                    min = apply(bodyfat, 2, min),
                    max = apply(bodyfat, 2, max)))
               
tmp
write.csv(tmp, file = "bodyfat_specs.csv")

model <- lm(Percent.body.fat.using.Brozek~., data=bodyfat)
prediction <- predict(model, newdata=bodyfat)
cor(prediction, bodyfat$Percent.body.fat.using.Brozek)
summary(model)

modelAbdomen <- lm(Percent.body.fat.using.Brozek~Abdomen.circumference, data=bodyfat)

plot(Percent.body.fat.using.Brozek~Abdomen.circumference, data=bodyfat)
abline(modelAbdomen, col="red")

plot(Percent.body.fat.using.Brozek~., data=bodyfat)
abline(modelAbdomen, col="red")
