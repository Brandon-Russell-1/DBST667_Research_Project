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