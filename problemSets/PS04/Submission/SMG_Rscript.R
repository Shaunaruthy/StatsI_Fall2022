#Packages and viewing/ exploring the data:

install.packages("car")
library(car)

data("Prestige")
help(Prestige)

#Type:
#Categorical
#bc, Blue Collar; prof, Professional, 
#Managerial, and Technical; wc, White Collar

#the first column is job title


library(tidyverse)

class(Prestige)

glimpse(Prestige)

attach(Prestige)
plot(income, prestige, main="Prestige and Income", 
     xlab="income", 
     ylab="prestige",
     col = Prestige$type,
     abline(lm(Prestige$prestige ~Prestige$income)),
     pch=19)

#Question 1:
#Create a new variable professionals = 1, and 
#Blue and white collar workers = 0

Professional = ifelse(Prestige$type == 'prof', 1, 0)
Professional

#Join the new variable to the data frame

Prestige <- cbind(Professional, Prestige)

#Question 1 (b) Run a linear model with 
#prestige as an outcome and income, professional, and the
#interaction of the two as predictors 
#(Note: this is a continuous  dummy interaction.)

#Remove NA values from the data frame:
Prestige <- na.omit(Prestige)

#Method 1 of creating the interaction variable:

#incomeI <- Prestige$income - mean(Prestige$income)
#professionalI <- Prestige$Professional - mean(Prestige$Professional)

#incprofI <- incomeI * professionalI

#model <- lm(prestige ~ income + Professional + incprofI, data=Prestige)

#summary(model)

#Method 2
#Linear regression including interaction variable:
model2 <- lm(prestige ~ income + Professional + income:Professional, data=Prestige)
summary(model2)

#Question 1 (c)
#Write the prediction equation based on the result.

ŷ = b0 + b1X1 + b2X2 + b3X1X2

Prestige = 21.14 + .003(income) + 
  37.78(professional)+ (-.002)(income)(professional)


#Question 1 (d)
#Interpret the coefficient for income.

Prestige = 21.14 + .003(income) 
+ 37.78(professional)
+ (-.002)(income)(professional)

y = 21.14  + .003(income) for White and blue Collar
 
y = (21.14 + 37.78 ) + (.003 - .002)(income) for professionals

For white and blue collar workers:
Income = 21.14 - (prestige) / .003

For professionals:
Income = 58.92 - (prestige) / .001



#Interpret the coefficient for professional

y = 21.14  + .003(income) for White and blue Collar

y = (21.14 + 37.78 ) + (.003 - .002)(income) for professionals

y = 58.92 + .001(income) for professionals


#Question 1 (f)
#What is the eect of a $1,000 increase in income on 
#prestige score for professional occupations? 
#In other words, we are interested in the marginal eect 
#of income when the variable professional takes the value of 1. 
#Calculate the change in ^y associated
#with a $1,000 increase in income based on your answer for (c).


Prestige = 21.14 + .003(income) 
+ 37.78(professional)
+ (-.002)(income)(professional)

prestige = 24.14 + 37.78(professional) -2(professional)

White Collar and Blue Collar:
  
Prestige = 24.14

For professionals:
Prestige = 59.92


#Question 1 - g
#What is the eect of changing one's occupations from 
#non-professional to professional
#when her income is $6,000? We are interested in the 
#marginal eect of professional
#jobs when the variable income takes the value of 6; 000. 
#Calculate the change in ^y
#based on your answer for (c).

Prestige = 21.14 + .003(income) + 
  37.78(professional)+ (-.002)(income)(professional)

#Income = 6000 Profession = 0 = non professional

Prestige = 21.14 + (.003)(6000) + 37.78(0)+(-.002)(6000)(0)

Prestige = 39.14

#income = 6000 Profession =1 = professionals

Prestige = 21.14 + (.003)(6000) + 37.78 (1) + (-.002)(6000)(1)

Prestige = 64.92

The change in y = 25.78





#Question 2
#Use the results from a linear regression to determine 
#whether having these yard signs
#in a precinct affects vote share 
#(e.g., conduct a hypothesis test with p = :05)

#Question 2 (a)

Hypotheis test:
  Null H0 : b3 = 0
  Alt Ha : b3 not = 0
  
Test statistic:
  t = b3 hat - 0 / seB3
  t = .042 - 0 / .016
  t = 2.625
  
P value:
  df = n -3 (3 variables in model)
  df = 128 
  
The p value based on these parameters:
 p = 0.00972

Based on the p value threshold of .05
we reject the null hypothesis that having these yard signs
in a precinct does not affect vote share.

y = .302 + .042b1 + .042b2



#Question 2 (b)

#Use the results to determine whether 
#being next to precincts with these yard signs
#affects vote share 
#(e.g., conduct a hypothesis test with  = :05).

Hypotheis test:
  Null H0 : b3 = 0
Alt Ha : b3 not = 0

Test statistic:
  t = b3 hat - 0 / seB3
t = .042 - 0 / .013
t = 3.231

P value:
  df = n -3 (3 variables in model)
df = 128 

The p value based on these parameters:
  p =  0.001568

Based on the p value threshold of .05
we reject the null hypothesis that being in a precint area adjacent
to yard signs does not affect vote share.


#Question 2 (c)
#Interpret the coecient for the constant term substantively.

y = .302 + .042b1 + .042b2

b0 is the constant

b0 = y - .042(b1 + b2)

The constant coefficient is the value at which the regression model
crosses the y axis. This is known as the y-intercept. 
When the variables b1 and b2 are equal to 0 the line intercepts 
the y axis at .302.


