#PS2- Question 1 Part a

#First create your variables including row and column totals
upper_Class <- c(14, 6, 7)
lower_Class <- c(7, 7, 1)
tot_ucrow <- sum(upper_Class)
tot_lcrow <- sum(lower_Class)
col_tot1 <- sum(14, 7)
col_tot2 <- sum(6, 7)
col_tot3 <- sum(7,1)

# Calculate the expected frequencies by dividing the row total
# by the grand total of the rows and multiplying by the column total

fe_1<- tot_ucrow/sum(tot_lcrow, tot_ucrow)*col_tot1
fe_2 <- tot_ucrow/sum(tot_lcrow, tot_ucrow)*col_tot2
fe_3 <- tot_ucrow/sum(tot_lcrow, tot_ucrow)*col_tot3

fe_4 <- tot_lcrow/sum(tot_lcrow, tot_ucrow)*col_tot1
fe_5 <- tot_lcrow/sum(tot_lcrow, tot_ucrow)*col_tot2
fe_6 <- tot_lcrow/sum(tot_lcrow, tot_ucrow)*col_tot3

#complete your test statistic by subtracting the expected frequency
# from the observed frequency. Square this number and divide by the expexted frequency.

a <- ((14-fe_1)*(14-fe_1)/fe_1)
b <- ((6-fe_2)*(6-fe_2)/fe_2) 
c <- ((7-fe_3)*(7-fe_3)/fe_3)
d <- ((7-fe_4)*(7-fe_4)/fe_4)
e <- ((7-fe_5)*(7-fe_5)/fe_5)
f <- ((1-fe_6)*(1-fe_6)/fe_6)

#sum these values

x2 <- sum(a,b,c,d,e,f)

##Therefore my X2 test statistic is 3.79


# Question 1 Part b

# First we work out the degrees of freedom
#df = (rows - 1)(columns - 1)

df <- (2-1)*(3-1)
df

#Our df = 2

#Now we can work out our p value

p_Value = pchisq(x2, df=2, lower.tail=F)

p_Value

#> p_Value
[1] 0.1502306


#We can see that p is greater than .1 (alpha) 
#so we fail to reject the null hypothesis that officers were neither more nor 
# less likely to solicit a bribe from drivers depending on their class

#Question 1- Part c

#We now need to find the adjusted residuals

# 0.165198209	-1.094519872	1.100135481
# -0.201444062	1.262233821	-1.304459049

# Quesion 1- d
# A residual can help us determine the vertical distance. 
# If the data point is above the line we receive a positive number and 
# if it's below that line of best fit we get a negative number.
# If the residual is close to 0 we can see the model is a good fit for the data
#as 0 indicates our guess or predicted value was a match.
# The residuals for this data are not close to 0 indicating that the predicted values
# are not close to the expected values. I have plotted these to demonstrate
# how far the predicted values are from the 0 line. The further from the 0 line they 
#are the less accurate the guess or prediction is. 

# Now we need our standardized residuals = fobserved - fexpected / se

14 - fe_1 / sqrt(fe_1(1-(tot_lcrow/42)(1-col_tot1/42)

residuals <- c(0.165198209,	-1.094519872,	1.100135481, 
-0.201444062,	1.262233821,	-1.304459049)

expected <- c(fe_1, fe_2, fe_3, fe_4, fe_5, fe_6)

plot(expected, residuals,
     main="Expected Vs Residual Values",
     xlab="Expected Observation", 
     ylab="Predicted Observation", pch=19)


#Question 2- Part a

#(a) State a null and alternative (two-tailed) hypothesis.
#The authors hypothesize that female politicians


#Question 2 - part b

library(readr)

data <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
#First I must work out the mean and sum of my variables

mean(data$reserved)
# x -.34
sum(data$reserved)
# sumx = 108
mean(data$water)
#y - 17.84
sum(data$water)
# sumy = 5745

sum(data$water-mean(data$water))^2

## 2.47

s <- sum((data$water =
    mean(data$water))
    * (data$reserved=
      mean(data$reserved)))
s

## 5.98

B <- 5.98 / 2.47
B

#B = 2.42


2.42*.34

A <- 17.84 - (.82)

A

#A = 17.02

A-B


##Use the lm function to check your coefficient values A and B

summary(lm(data$water ~ data$reserved, data = data))


#Question 2 - Part C
#The estimated coefficient value indicates that when x= 0 this is our y value
#In this case if the reserved policy = 0 or is not present we see a score of
#14.74 in our water measure.

      