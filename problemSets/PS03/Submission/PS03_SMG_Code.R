class(incumbents_subset)

#Regressions OUTCOME = VOTESHARE and EXPLANATORY = DIFFLOG

difflog_Voteshare <- lm(voteshare ~ difflog, data = incumbents_subset)
summary(difflog_Voteshare)


#Scatterplot with regression line:

plot(incumbents_subset$difflog, incumbents_subset$voteshare, main = "Voteshare and Difflog",
     xlab = "difflog", ylab = "voteshare",
     pch = 19, frame = FALSE)
abline(lm(voteshare ~ difflog, data = incumbents_subset), col = "blue")


#Residuals in a separate object:

difflog_VoteshareRes <- difflog_Voteshare["residuals"]


#Write the prediction equation:

y = Bo + B1X + e

y = .579 + .042(X) + .001


#Question 2
#Linear Regression OUTCOME = PRESVOTE EXPLANATORY = DIFFLOG

presvote_Diffloglm <- lm(presvote ~ difflog, data = incumbents_subset)
summary(presvote_Diffloglm)

#Scatterplot with R line:

plot(incumbents_subset$difflog, incumbents_subset$presvote, main = "President Vote and Difference in Spending",
     xlab = "Difference in Spending", ylab = "President Vote",
     pch = 19, frame = FALSE)
abline(lm(presvote ~ difflog, data = incumbents_subset), col = "blue")


#Save the residuals in a seperate Object:

difflog_PresvoteRes <- presvote_Diffloglm["residuals"]
difflog_PresvoteRes


# Write the Prediction Equation:
y = .508 + .024(X) + .001

# Question 3
#Regression OUTCOME = VOTESHARE EXPLANATORY = PRESVOTE

voteshare_Presvote <- lm(voteshare ~ presvote, data = incumbents_subset)

summary(voteshare_Presvote)

#Scatterplot with Regression Line:

plot(incumbents_subset$presvote, incumbents_subset$voteshare, main = "President Vote and Incumbent Success",
     xlab = "PresVote", ylab = "Incumbent Success",
     pch = 19, frame = FALSE)
abline(lm(voteshare ~ presvote, data = incumbents_subset), col = "blue")

# Write the prediction equation

y = .441 + .388(X) + .013

#Question 4 Residual Regression Model:

regressionlm <- lm(unlist(difflog_VoteshareRes) ~ unlist(difflog_PresvoteRes), data = incumbents_subset)

summary(regressionlm)

#Scatterplot of residuals:

plot(unlist(difflog_PresvoteRes), unlist(difflog_VoteshareRes), main = "Residual Plot",
     xlab = "Diff/PresVote Residuals", ylab = "Diff/Voteshare Residuals",
     pch = 19, frame = FALSE)
abline(lm(unlist(difflog_PresvoteRes) ~ unlist(difflog_VoteshareRes), data = incumbents_subset), col = "blue")

#Write the prediction equation:

y = -5.207 + 2.569(X) + 1.176


#Question 5
#OUTCOME=VOTESHARE
#EXPLANATORYS = DIFFLOG AND PRESVOTE

model <- lm(voteshare ~ difflog + presvote, data = incumbents_subset)
summary(model)

#Write the prediction equation:

Voteshare = .449 + .036(difflog) + .257(presvote)

#Presvote t value and difflog/presvote res t value = 21.84



