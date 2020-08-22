# Chapter 17: More on Simple Regression
# good 2 haves

rm(list = ls())  # Clean up global environment 
dev.off()        # Remove plots

# Let's read the data into RStudio
gdp <- read.table('GDP.txt', sep="\t", header = T)

# Now show the first 5 rows
head(gdp)
# And the name of the variables
names(gdp)
# Finally let's analyze the structure of the dataframe
str(gdp)
# We have 3 integer variables and 1 object variable 
# with 51 rows and 4 columns.

# We are asked to make a histogram and QQ plot with $Personal.Income
# to check the Normal Population Assumption. If the assumption
# is violated, use of one of the ladder powers.

# Let's set a plot of 1 by 2.
par(mfrow= c(1,2))
hist(gdp$Personal.Income, main = "$Personal.Income")
qqnorm(gdp$Personal.Income)
qqline(gdp$Personal.Income)

# We see that the distribution of $Personal.Income is highly
# skewed to the right. We therefore should try to use one
# of the ladder of powers.

# Let's make a plot of 4x4 to find which power gives a linear form to $Personal.Income
par(mfrow= c(2,2))
# Square root of $Personal.Income 
qqnorm(sqrt(gdp$Personal.Income),
       main = expression(paste("QQ plot of ", sqrt(Personal.Income)))) 
qqline(sqrt(gdp$Personal.Income))
# Log10($Personal.Income)
qqnorm(log10(gdp$Personal.Income), main = "QQ plot of Log10(Personal.Income)") 
qqline(log10(gdp$Personal.Income))
# -1/square root of $Personal.Income
qqnorm(-1/sqrt(gdp$Personal.Income),
       main = expression(paste("QQ plot of ", -1/sqrt(Personal.Income))))
qqline(-1/sqrt(gdp$Personal.Income))
# -1/$Personal.Income
qqnorm(-1/(gdp$Personal.Income), main = "QQ plot of -1/Personal.Income") 
qqline(-1/(gdp$Personal.Income))

# Based on the QQ plots, we choose to use Log10 transformation since
# it gives $Personal.Income more linearity

# Now, we are asked to create a scatter plot of $Personal.Income against
# $GDP and check for any assumption that are being violated.

# Scatter plot $Personal.Income ~ $GDP

# log10 of $Personal.Income against $GDP
plot(log10(Personal.Income) ~ GDP, data= gdp,
     xlab="GDP",
     ylab= "Log10(Personal.Income")

# log10 of $Personal.Income against sqrt of $GDP
plot(log10(Personal.Income) ~ sqrt(GDP), data= gdp,
     xlab=expression(sqrt(GDP)),
     ylab= "Log10(Personal.Income")

# log10 of $Personal.Income against log10 of $GDP
plot(log10(Personal.Income) ~ log10(GDP), data= gdp,
     xlab="Log10(sqrt(GDP)",
     ylab= "Log10(Personal.Income")

# log10 of $Personal.Income against -1/sqrt of $GDP
plot(log10(Personal.Income) ~ -1/sqrt(GDP), data= gdp,
     xlab=expression(-1/sqrt(GDP)),
     ylab= "Log10(Personal.Income")

# Looking at the plots we just created, many of the transformations
# follow a concave downward patter except for log10. We again choose
# the Log10 transformation since it gives a linear patter between
# $GDP and $Personal.Income.

# We are asked to create a linear regression model with the transformed
# variables, $GDP and $Personal.Income. Check if the Equal Variance is violated.

# Reg Model
imod <- lm(log10(Personal.Income) ~ log10(GDP), data= gdp)
summary(imod)

# Reset plots to one
par(mfrow= c(1,1))
plot(imod$fitted.values, imod$residuals, xlab="Fitted Value", ylab="Residual")
abline(a=0, b=0)

# We see that the model does not violate any of the regression model assumptions.
# Our model is fairly linear and the equal variance is satisfied since
# the residual points in the plot are spread out. However, we do see some
# outliers in the plot but it doesn't affect the spread of residuals.

# Now, let's get a closer look of these outliers and check whether these outliers
# are in fact giving the model some leverage and check if the outliers are influential.

# We are going to use the order function in R.
# The order function does what the name is. It basically tells you
# how to get your data in ascending order.

# Since we want to discern the two outliers in the plot,
# we should use the order function to select them order by their residual value.
ord <- order(imod$residuals)
View(ord)
gdp[ord[1],] # District of Columbia
gdp[ord[2],] # Delaware

# Now let's remove them from the regression model and create a new one
# without them

gdp.new <- gdp[-c(ord[1], ord[2]) ,] # new data without outliers

#### Using Cooks Distance 
# mod <- lm(log10(Personal.Income) ~ log10(GDP), data= gdp)
# cooksd <- cooks.distance(mod)
# 
# plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
# 
# influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
# head(gdp[influential, ])  # influential observations.
####

# Now we create a new regression model with the new data w/o outliers.
imod.new <- lm(log10(Personal.Income) ~ log10(GDP), data=gdp.new)

plot(log10(gdp$GDP), log10(gdp$Personal.Income)
     , xlab = 'Log10(GDP)'
     , ylab = 'Log10(Personal.Income)')
abline(imod, col = 'red') 
abline(imod.new, col = 'green')

# Things that we learned from this plot:
   # Both residuals are outliers but they do not have a high leverage since their GPD value is close to the mean GDP
   # Both regression models are very similar
   # Thus the outliers are not influential at all!

# Suppose there's a state with GDP of 300,000. Can we predict personal income?
# If so, use a 95% prediction interval.

# First, verify GDP range. 
range(gdp$GDP)
# We can since range starts from 19,713 to 1,457,090.
# Create new data with GDP of 300,000
pred.data <- data.frame(GDP = 300000)
View(pred.data)
result <- predict(imod, newdata = pred.data, interval = 'prediction', level = 0.95)
10^result # Recall that use log10 on both variables.

# We see that the predicted personal income for that state is 288544.1 and its
# 95% prediction interval is (218840.9, 380448.5)


################################
# just for curiosity lets use the shapiro-test
# null-hypothesis: the data is normally distributed
# if p-value is greater than 0.05, normality can be assumed

shapiro.test(gdp$Personal.Income)
# p = 4.976e-09 = 0.000000004976
# strong evidence of non-normality can be assumed 
##############################


# CLEAN UP #

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
