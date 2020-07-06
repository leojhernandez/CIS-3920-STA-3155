# Chapter 16: Inference for Simple Regression
# good 2 haves
rm(list = ls())  # Clean up global environment 
dev.off()        # Remove plots

# Let's import the data!
pizza <- read.table('Frozen_Pizza.txt', sep= '\t', header=TRUE)

# Let's look at the structure of the dataframe
head(pizza)
names(pizza)
str(pizza)

# We have 9 variables, all quantitative except for $Week.
# We are asked to plot sales as our response variable and price as the predictor variable
# for each city. Then we can we create four linear regression models to see which city sales 
# tends to be more sentivity to price.
par(mfrow = c(2,2))

plot(Baltimore.Volume ~ Baltimore.Price, data=pizza, main="Baltimore")
plot(Dallas.Volume ~ Dallas.Price, data=pizza, main="Dallas")
plot(Chicago.Volume ~ Chicago.Price, data=pizza, main="Chigaco")
plot(Denver.Volume ~ Denver.Price, data=pizza, main="Denver")

# We see that all plots follow a negative slope. Chicago and Baltimore
# with a few outliers. We need to dig deeper to find more information.
# Let's create linear models for each plot to learn more

balt.mod <- lm(Baltimore.Volume ~ Baltimore.Price, data = pizza)
dall.mod <- lm(Dallas.Volume ~ Dallas.Price, data = pizza)
chig.mod <- lm(Chicago.Volume ~ Chicago.Price, data = pizza)
denv.mod <- lm(Denver.Volume ~ Denver.Price, data = pizza)

# Let's collect their coefficients to find which city is more sentitive to price.
balt.mod$coefficients # y~hat = 126,624.9 - 34,955.6 * Price
dall.mod$coefficients # y~hat = 139,547.43 - 33,527.19 * Price
chig.mod$coefficients # y~hat = 1,094,047.1 - 331,151.8 * Price
denv.mod$coefficients # y~hat = 181,218.15 - 52,795.75 * Price
# We can quickly point out that Chicago sales is very sentivity to price because
# it has the smallest slope (b1 = -331,151.8). In other words, as the price increase
# by one dollar, chicago sales decreases dramatically.

# Now, let's plot a residual plot and QQ plot for the models we just created
# to see if any of the regression assumptions were violated.

# Recall regression/model assumptions:
  # Linearity         -- plot e against x or y~hat
  # Independence      -- met if sample is collected randomly.
  #                   -- If data is a time-series, 
  #                   -- plot e agaist its order (not x or y~hat)
  # Normal Population -- do QQ plot and check for end tails
  # Equal Variance    -- plot e against x or y~hat. Should be constant.

# Format plot to 1 row and 4 columns
par(mfrow = c(1,4))

# Baltimore plots
plot(balt.mod$residuals, main = 'Baltimore', xlab = 'Time', ylab = "Residual")
abline(a = 0, b = 0)
plot(balt.mod$fitted.values, balt.mod$residuals, xlab = "Fitted value",
     ylab = "Residual", main = 'Baltimore')
abline(a = 0, b = 0)
qqnorm(balt.mod$residuals, main = 'Baltimore')
qqline(balt.mod$residuals)
hist(balt.mod$residuals, xlab = 'Residual', main = 'Histogram of Residuals')
#Summary:
# In the Baltimore plots, we see that both linearity and constant variance
# assumptions are violated in the residual plot, showing a negative trend 
# and the number of points increase as we move downward. The independence 
# assumption is also violated in the plot showing an upward and downward pattern.
# Finally, the normal population assumption is violated because in the QQ and 
# histogram plot shows a right/positive skew distribution. We see that some 
# points are way above the line thus violating the normality distribution.


# Chicago plots
plot(chig.mod$residuals, main = 'Chicago', xlab = 'Time', ylab = "Residual")
abline(a = 0, b = 0)
plot(chig.mod$fitted.values, chig.mod$residuals, xlab = "Fitted value",
     ylab = "Residual", main = 'Chicago')
abline(a = 0, b = 0)
qqnorm(chig.mod$residuals, main = 'Chicago')
qqline(chig.mod$residuals)
hist(chig.mod$residuals, xlab = 'Residual', main = 'Histogram of Residuals')
#Summary:
# In the Chicago plots, we see that the linearity assumption is violated 
# because in the residual plot shows a negative downward slope but the 
# constant variance assumption seems to pass if remove the two outliers 
# the plot. The independence assumption is satisfied because the points look random.
# Finally, the normal population assumption is violated because in the QQ and 
# histogram plot shows a right/positive skew distribution. We see that those 
# two points are above the line thus violating the normality distribution.


# Dallas plots
plot(dall.mod$residuals, main = 'Dallas', xlab = 'Time', ylab = "Residual")
abline(a = 0, b = 0)
plot(dall.mod$fitted.values, dall.mod$residuals, xlab = "Fitted value",
     ylab = "Residual", main = 'Dallas')
abline(a = 0, b = 0)
qqnorm(dall.mod$residuals, main = 'Dallas')
qqline(dall.mod$residuals)
hist(dall.mod$residuals, xlab = 'Residual', main = 'Histogram of Residuals')
#Summary:
# In the Dallas plots, we see that both linearity and constant assumption 
# are in fact satified because in the residual plots shows randomness.
# However, the independence assumption is violated because in the residual plot
# shows an upwards trend. Finally, the normal population assumption is also
# violated because in the QQ and histogram plot shows a right/positive skew distribution.


# Denver plots
plot(denv.mod$residuals, main = 'Denver', xlab = 'Time', ylab = "Residual")
abline(a = 0, b = 0)
plot(denv.mod$fitted.values, denv.mod$residuals, xlab = "Fitted value",
     ylab = "Residual", main = 'Denver')
abline(a = 0, b = 0)
qqnorm(denv.mod$residuals, main = 'Denver')
qqline(denv.mod$residuals)
hist(denv.mod$residuals, xlab = 'Residual', main = 'Histogram of Residuals')
#Summary:
# In the Denver plots, we see that both linearity and independence assumption
# are satisfied because the residual plots shows randomness. However, in the 
# constant variance assumption, it's difficult to tell if the assumption is satisfied
# as the residual plot shows an unsual shape. More research and work need to be done.
# Finally, the normal population assumption is violated because in the QQ and 
# histogram plot shows a right/positive skew distribution.


# Let's focus on the model for Dallas. Show a 90% CI for the slope $Price 
# and interpret it. Based on the interval, can we say there is statistically 
# significant linear relationship between $Price and $Sales?

# Let's use the confint function on the Dallas model!
?confint
confint(dall.mod, level = 0.90)

# The 90% confidence interval is (-40655.79, -26398.58). So we can say,
# we are 90% confident as the unit price increases by one dollar the sales of
# pizza will be lower on average between 40655.79 and 26398.58 volume.
# There is statistically significance between $Price & $Sales because the point
# of difference does not cross 0 therefore we say it is statistically significant.


# Conduct a hypothesis test to see if there is a negative correlation between 
# $Price and $Sale volume in the city Dallas
# In this case, Ho: B1 = 0 , Ha: B1 < 0
# Run the summary function:
summary(dall.mod)

# We are interested in the t-test and p-value:
summary(dall.mod)$coeff
# Dallas.Price t-test = -7.782879 & p-value = 9.618115e-13
# We need the p-value for one side where B1 < 0, so we
# divided 9.618115e-13/2  ... (9.618115/2 = 4.809) ...
# in which we get 4.809e-13. 
# We reject the Ho test and say that there is negative correlation


# Suppose we want to estimate the mean Sales if the Price is $2.50 and $3.00 
# using 95% confidence intervals for the city of Dallas.
# Interpret both intervals. Can we also estimate the mean Sales if the 
# Price is $3.50? Explain.

# Let's create a new dataset that contains both prices and use it to
# estimate the mean sales. 

price <- data.frame(Dallas.Price = c(2.5, 3))
predict(dall.mod, newdata = price, interval = 'confidence')
#   fit      lwr      upr
# 1 55729.47 54063.14 57395.79  # Price at $2.5
# 2 38965.87 35464.31 42467.44  # Price at $3

# The mean sales is 55729.47 when the price is $2.5 and the confidence interval 
# is (54063.14, 57395.79). 

# The mean sales is  when the price 38965.87 is $3 and the confidence interval 
# is (35464.31, 42467.44). 

# Can we predict the mean sales of pizza when price is $3.5? 
# The answer is no. Because $3.5 is outside the range of the sampled prices.
range(pizza$Dallas.Price)
# [1] 2.21 3.05

# For city Dallas we know the pizza price was $2.77 in the last week of 1996.
# Suppose the price would increase to $2.99 in the following week. 
# Can we predict the sales for that week and account for the uncertainty of
# your prediction? Do you think the resulting prediction is useful?

# Let's use the same technique we used above

newprice <- data.frame(Dallas.Price = c(2.99))
predict(dall.mod, newdata = newprice, interval = 'predict')
#   fit      lwr      upr
# 1 39301.15 22425.65 56176.65

# The mean sales is 39301.15 when the price is $2.99 and the confidence interval 
# is (22425.65, 56176.65). 

# We do not think this is a useful prediction because it's interval is too wide.
# Let's plot this. But first reset the plot.

par(mfrow = c(1,1))

xx <- seq(min(pizza$Dallas.Price), max(pizza$Dallas.Price), length.out = 200)
xx

new.band <- data.frame(Dallas.Price = xx)
new.band

conf <- predict(dall.mod, newdata = new.band, interval = 'confidence', level = .95)
pred <- predict(dall.mod, newdata = new.band, interval = 'prediction', level = .95)

plot(pizza$Dallas.Price, pizza$Dallas.Volume, xlab = 'Dallas Price', ylab = 'Dallas Volume')
abline(dall.mod)

lines(xx, conf[, 'lwr'], lty = 2, col = 'red')
lines(xx, conf[, 'upr'], lty = 2, col = 'red')
lines(xx, pred[, 'lwr'], lty = 3, col = 'blue')
lines(xx, pred[, 'upr'], lty = 3, col = 'blue')

# As we can see, the prediction band is much wider than the confidence band,
# and it covers almost all the data points. Some data points are actually
# out of the prediction band which can be potential outliers for the model.
# Both intervals actually get narrower as x-values approach their average, 
# but wider as the x-values move away from it.


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
