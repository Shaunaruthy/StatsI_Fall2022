
library(tidyverse) 
library(broom) 


dat <- read.csv("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv",
  sep = "\t")

class(dat)

plot(dat)
library(dplyr)

dat <- dat %>%
  mutate(Ddate = as.Date(dat$DocumentDate))

cor(dat$SqFtTotLiving, dat$SalePrice)
plot(dat$SqFtTotLiving, dat$SalePrice)

plot(dat$Bedrooms, dat$SalePrice)

cor(as.numeric(dat$NewConstruction), dat$SalePrice)

hist(dat$ZipCode, dat$SalePrice)

?model.matrix()

#intervals to category: eg bedrooms to bathroom. Ext outliers
#we might need to cut it into a categorical variable
#cut function in r does this - specify n of cat 
#you want to increase your predictive power of the model aim for a good distribution of the data

#Bathrooms / bedrooms etc experiment with these

#Categories to intervals: buildgrade: we have 11 categories
#10 terms needed if you keep it cate makes sense to go interval

library(tidyverse)
library(dplyr)

dat %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Zip Code")


zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

##library(ggplot2)
##ggplot(zip_group, aes(x=ZipCode, y=ZipGroup)) +  geom_boxplot(fill='green')

install.packages("stargazer")
library(stargazer)

class(zip_group)

dat <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")
mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)

#as categorical

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + as.factor(ZipGroup), data = dat)


mod5 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + as.factor(ZipGroup) + TrafficNoise, data = dat)

stargazer(mod5, type = "text")



