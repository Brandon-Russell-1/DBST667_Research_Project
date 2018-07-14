#Body Fat dataset Research Project Analysis
#Brandon Russell - DBST667
#Various figure and table builder coding sections

#Build distributions for Figure 1
library(ggplot2)
library(tidyr)

#Build figure 1 for paper
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

#Boxplot for outlier analysis, figure 2
boxplot(bodyfat, las = 2, par(mar = c(12, 5, 4, 2)+ 0.1))
set.seed(482)

boxplot(bodyfat$Weight)
identify(rep(1, length(bodyfat$Weight)), bodyfat$Weight, labels = seq_along(bodyfat$Weight))

boxplot(bodyfat$Abdomen.circumference)
identify(rep(1, length(bodyfat$Abdomen.circumference)), bodyfat$Abdomen.circumference, labels = seq_along(bodyfat$Abdomen.circumference))

boxplot(bodyfat$Chest.circumference)
identify(rep(1, length(bodyfat$Chest.circumference)), bodyfat$Chest.circumference, labels = seq_along(bodyfat$Chest.circumference))

boxplot(bodyfat$Hip.circumference)
identify(rep(1, length(bodyfat$Hip.circumference)), bodyfat$Hip.circumference, labels = seq_along(bodyfat$Hip.circumference))
