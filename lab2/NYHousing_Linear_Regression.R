library("ggplot2")
library("readr")



## read dataset
NY_House_Dataset <- read_csv("/Users/Shanelle/School/Spring26/Data_analytics/lab2/NY-House-Dataset.csv")
View(NY_House_Dataset)
dataset <- NY_House_Dataset

## filter data
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]


#Model 1:
lmod1 <- lm(log10(PRICE)~log10(PROPERTYSQFT) + BEDS, data = dataset)
summary(lmod1)

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() + 
  stat_smooth(method='lm',col='red')

ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

#Model 2:
lmod2 <- lm(log10(PRICE)~log10(PROPERTYSQFT)+BATH, data=dataset)
summary(lmod2)

ggplot(dataset, aes(x =log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red") 

ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


#Model 3: 
lmod3 <- lm(log10(PRICE) ~ BEDS + BATH, data = dataset)
summary(lmod3)

ggplot(dataset, aes(x = BATH, y=log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") 

ggplot(lmod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

### THE END ###

