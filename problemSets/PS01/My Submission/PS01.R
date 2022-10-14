
# set working directory
setwd("C:\\Users\\shauna-mcgeever\\Documents\\GitHub\\StatsI_Fall2022\\problemSets\\PS01\\template")


#####################
# Problem 1
#####################

##Problem 1 - Part 1 
##Find a 90\% confidence interval for the average student IQ in the school.

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
print(y)

CI_lower <- qnorm(0.05, 
                  mean = mean(y), 
                  sd = (sd(y)/sqrt(length(y))) 
)

CI_upper <- qnorm(0.95,
                  mean = mean(y),
                  sd = (sd(y)/sqrt(length(y)))
)

matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))





#Question 1 part 2: First visualize the data

class(y)
par("mar")
par(mar=c(1,1,1,1))

boxplot(y, main ="Boxplot of IQ", ylab = "IQ", xlab= "")

#Find the mean of y

meany<- mean(y)

#Find the sum of the demeaned values in this data set. 
#Then find the sum of the errors.

demSum <- y - mean(y)
demSum
sumErrors <- sum(demSum)

SQE <- sumErrors*sumErrors
sum(SQE)

# Use this value to work out the variance in the data. 
# The square root of this is the standard deviation.
s2 <- sum(SQE)/24

standardDeviation <- sqrt(s2)
standardDeviation

# Use your test statistics to work out your t value.
# t = sample mean- population mean divided by SD/sqroot(n-1).
(standardDeviation/sqrt(24))

t<- (meany-100)/2.37

t

# Our t value is lower that the critical value on our t table.
# We fail to reject the null hypothesis. 
#The mean IQ for the class is not higher than the population mean.
# Conduct a t-test to check your answer.

t.test(y,
       mu = 100, 
       var.equal = FALSE, 
       alternative = "greater", 
       conf.level = .95)




#####################
# Problem 2
#####################
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header = TRUE)
expenditure
ls(expenditure)

plot(expenditure$Region, expenditure$Y)
legend("topleft", legend = c("1=NorthEast", 
                             "2=North Central", "3=South", "4=West"))


plot(expenditure$X1, expenditure$Y,
     xlab= "per capita personal income in state",
     ylab = "per capita expenditure on shelters or housing assistance in state",
)
#This chart demonstrates that the more personal income in a state the more money that
# is spent on housing and shelter assistance in a state. Low income is associated with low
# spending in this area.


plot(expenditure$X1, expenditure$X2,
     xlab= "per capita personal income in state",
     ylab= "Number of residents per 100,000 that are financially insecure in state")
#The highest number of people that are deemed financially insecure in a state
# are in the 1500 to 2000 dollar range of personal income.

plot(expenditure$X1, expenditure$X3,
     xlab = "per capita personal income in state",
     ylab = "Number of people per thousand residing in urban areas in state")
#We can see that the highest level of personal incomes are associated with 
#the highest number of people living in urban areas.

plot(expenditure$Y, expenditure$X2,
     xlab = "per capita expenditure on shelters or housing assistance in state",
     ylab = "Number of residents per 100,000 that are financially insecure in state")
#The lower the number of residents that are deemed financially insecure the lower spending
# is seen in relation to shelter and housing expenditure. The higher number of financially insecure
# individuals in a state is associated with higher spending in relation to shelter and housing assistance.


plot(expenditure$Y, expenditure$X3,
     xlab = "per capita expenditure on shelters or housing assistance in state",
     ylab= "Number of people per thousand residing in urban areas in state",
)
#Low levels of shelter or housing assistance expenditure is associated with lower
# populations of people residing in urban areas. The more individuals residing in the
# urban areas of states the higher the costs in relation to housing assistance.

plot(expenditure$X2, expenditure$X3,
     xlab = "Number of residents per 100,000 fin insecure in state",
     ylab= "Number of people per thousand residing in urban areas in state",
)

#We can see here that there is not a strong relationship between these two variables.
#Lower numbers of individuals in financially insecure states occur both in high and low
# numbers of individuals living in urban areas.

##Please plot the relationship between \emph{Y} and \emph{Region}? On average, 
##which region has the highest per capita expenditure on housing assistance?


plot(expenditure$Region, expenditure$Y)
legend("topleft", legend = c("1=NorthEast", "2=North Central", "3=South", "4=West"))

expenditure$Region


#The west region has the highest expenditure on housing on average.


##plot the relationship between \emph{Y} and \emph{X1}? Describe this graph and the relationship. 
##Reproduce the above graph including one more variable \emph{Region} and display different regions with different types of symbols and colors.




cols <- c("pink", "green", "yellow","blue")



plot(expenditure$Y, expenditure$X1,
     xlab = "expenditure on shelters/housing assistance in state",
     ylab = "personal income in state",
     col = cols[expenditure$Region], pch = 19)

legend("bottomright", legend = levels(factor(expenditure$Region)), 
       col = expenditure$Region)

#We can see that higher levels of personal income in a state are associated with higher spending
# on shelters or housing assistance. The lower the personal income in the state dramatically reduces this spending.

install.packages("knitr")
install.packages("ggplot2")

#Region is now added to this chart using color as a representative.

