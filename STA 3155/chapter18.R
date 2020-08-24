## Chapter 18: Multiple Regression
# good 2 haves
rm(list = ls())  # Clean up global environment 
dev.off()        # Remove plots

# Let's read the data into RStudio
motor <- read.table('Motorcycles.txt', sep= '\t', header = TRUE)

# Read first 5 rows
head(motor)

# We have 4 numerical variables and 1 categorical variable
str(motor)
# Dimension of the data frame
dim(motor)
# 93 rows and 7 columns in total

plot(motor)

# By looking at this plot, we can see that $MSRP which is the
# the market suggested retail price has a positive correlation 
# with many variables such as $Bore, $Displacement, $Clearance and $Wheelbase

# Let's use $MSRP as our response variable and take the other variables that
# are correlated as the predictor variables. Find which plot is most appropriate 
# for a simple linear regression.

par(mfrow= c(1,2))
# MSRP && Bore
plot(motor$Bore,motor$MSRP, xlab = "Bore", ylab = "MSRP")
plot(factor(motor$Bore),motor$MSRP, xlab = "Bore", ylab = "MSRP") 
# box plot with MSRP against Bore

# MSRP && Displacement
plot(motor$Displacement,motor$MSRP, xlab = "Displacement", ylab = "MSRP")
plot(factor(motor$Displacement),motor$MSRP, xlab = "Displacement", ylab = "MSRP") 
# box plot with MSRP against Displacement

# MSRP && Clearance
plot(motor$Clearance,motor$MSRP, xlab = "Clearance", ylab = "MSRP")
plot(factor(motor$Clearance),motor$MSRP, xlab = "Clearance", ylab = "MSRP") 
# box plot with MSRP against Clearance

# MSRP && Wheelbase
plot(motor$Wheelbase,motor$MSRP, xlab = "Wheelbase", ylab = "MSRP")
plot(factor(motor$Wheelbase),motor$MSRP, xlab = "Wheelbase", ylab = "MSRP") 
# box plot with MSRP against Wheelbase

# While all plots show a positive slope, not all fit the regression assumptions.
# The Displacement and Bore can be used as predictors since each plot show a linear pattern.
# The Clearance and Wheelbase plots show a curve thus violating the linearity assumption.


# Now let's build a multiple regression model w/$Displacement && $Bore as predictors
# Interpret their coefficients and report both R-squared and R-squared adjusted.
par(mfrow= c(1,1))

imod <- lm(MSRP ~ Displacement + Bore, data=motor)
summary(imod)
# Call:
# lm(formula = MSRP ~ Displacement + Bore, data = motor)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1582.7  -877.6  -178.2   805.6  1941.0 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   423.025   1036.588   0.408   0.6842  
# Displacement    6.722      3.324   2.022   0.0461 *
# Bore           38.915     26.221   1.484   0.1413  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 998.6 on 90 degrees of freedom
# Multiple R-squared:  0.7566,	Adjusted R-squared:  0.7512 
# F-statistic: 139.9 on 2 and 90 DF,  p-value: < 2.2e-16

# Our simple multiple regression models as follows:
#           y~hat(MSRP) = 423.025 + 6.722 * Displacement + 38.915 * Bore

# Multiple R-squared:  0.7566,	Adjusted R-squared:  0.7512 

# We can interpret this models as follows:
# The MSRP of a motorbike is expected to increase by 6.722 when the displacement increases by 1 after accounting for its bore in the model.
# The MSRP of a motorbike is expected to increase by 38.915 when the bore increases by 1 after accounting for its displacement in the model.

# Now let's check if the regression model fits the assumptions.
par(mfrow= c(1,1))

plot(imod$fitted.values, imod$residuals, xlab = "Fitted Values", ylab = "Residuals")
abline(0,0)
hist(imod$residuals)
qqnorm(imod$residuals)
qqline(imod$residuals)

# The linearity assumption is satisfied since the residual plot doesn't show any bends.
# The Independence assumption is satisfied since it's a random experiment and motorcycles are independent.
# The Normality assumption is satisfied since the distribution of residuals is unimodal and has no outlier.
# The Equal Variance Assumption is satisfied since the residual spread shows no pattern. 

# Conduct a test to see if the fitted multiple regression model is statistically usefu.
# If useful, check which predictors are making the model useful.

# We run the summary function again,
summary(imod)
# Call:
# lm(formula = MSRP ~ Displacement + Bore, data = motor)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1582.7  -877.6  -178.2   805.6  1941.0 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   423.025   1036.588   0.408   0.6842  
# Displacement    6.722      3.324   2.022   0.0461 *
# Bore           38.915     26.221   1.484   0.1413  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 998.6 on 90 degrees of freedom
# Multiple R-squared:  0.7566,	Adjusted R-squared:  0.7512 
# F-statistic: 139.9 on 2 and 90 DF,  p-value: < 2.2e-16

# Ho : β1 = β2 = ... = βk = 0 
# Ha : At least one  βi ≠ 0, i = 1, ..., k

# To check whether this regression model is useful, I'll do a Ho & Ha testing.
# MSRP = Bo + B1(Displacement) + B2(Bore)
#   If Ho: B1 = B2 = 0 then Displacement or Bore does not affect MSRP.
#   If Ha: At least B1 ≠ 0 or B2 ≠ 0, then either Displacement or Bore or both predictor affect MSRP.

# Looking at the summary, the F-statistic is 139.9 and the p-value: < 2.2e-16, we therefore reject Ho.
# And say that there is at least one useful predictor.

# Now let's look the summary output again, we find the p-value for both predictors:
#                     Pr(>|t|)  
#     Displacement    0.0461 *
#     Bore            0.1413  
# We reject Ho for Displacement but not Bore, where the level of significance is 5%
# as Bore p-value is greater than 0.05.
# Thus we say that Displacement makes a significant distribution to MSRP but Bore does not.


# Suppose we aren't satisfied with our model so we add a new predictor to the model.
# We remember that Clearance is correlated to MSRP but the problem was that there was unequal spread.
# Let's add Clearance to the model to see if both R-squared increase or not.

imod2 <- lm(MSRP ~ Displacement + Bore + Clearance, data=motor)
summary(imod2)
# Call:
# lm(formula = MSRP ~ Displacement + Bore + Clearance, data = motor)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1185.51  -474.74   -53.64   534.97  1646.54 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  -1595.007    711.276  -2.242  0.02742 *  
#   Displacement     7.420      2.202   3.370  0.00111 ** 
#   Bore             9.229     17.576   0.525  0.60082    
#   Clearance      314.908     29.193  10.787  < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 661.1 on 89 degrees of freedom
# Multiple R-squared:  0.8945,	Adjusted R-squared:  0.891 
# F-statistic: 251.6 on 3 and 89 DF,  p-value: < 2.2e-16

# We see that R-squared increased to 0.8945 from previous model 0.7566 and
# Adjusted R-squared increased to 0.891 from 0.7512. 
# However, we see that Bore becomes less significant where its p-value increased by 0.46 
# and it's slope decrease but the F-statistic became more significant, 251.6.
# We can conclude that Bore is not significant when is used with Displacement and Clearance it is significant.

# Now let's check for any assumptions
par(mfrow= c(1,1))

plot(imod2$fitted.values, imod2$residuals, xlab="Fitted Value", ylab="Residual")
abline(0,0)
hist(imod2$residuals, main = '', xlab = "Residual")
qqnorm(imod2$residuals)
qqline(imod2$residuals)

# We check all the assumptions! The residual showed residual with random patter.
# Both linearity and equal variance assumption are satisfied.
# The independence assumption is satisfied since this sample is a random experiment.
# The normality assumption is also satisfied but this assumption becomes less important 
# when we have a large sample, n = 93.

# What if we add Wheelbase to the model? Does it become more statistical useful?
imod3 <- lm(MSRP ~ Displacement + Bore + Clearance + Wheelbase, data=motor)
summary(imod3)

# Call:
# lm(formula = MSRP ~ Displacement + Bore + Clearance + Wheelbase, 
#      data = motor)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1191.70  -471.62   -68.88   514.01  1694.01 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1720.633    777.363  -2.213  0.02945 *  
# Displacement     7.786      2.385   3.265  0.00156 ** 
# Bore             4.889     20.580   0.238  0.81277    
# Clearance      299.343     47.929   6.246 1.46e-08 ***
# Wheelbase        9.996     24.345   0.411  0.68236    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 664.2 on 88 degrees of freedom
# Multiple R-squared:  0.8947,	Adjusted R-squared:  0.8899 
# F-statistic:   187 on 4 and 88 DF,  p-value: < 2.2e-16

# We see the R-squared is 0.8947 increased by very little (0.8945) but the opposite 
# for Adjusted R-squared from 0.8899 to 0.891.
# The F-statistic decreased by 60+ points. Thus we can say that Wheelbase is not significant
# and contributes very little to MSPR when the other predictors are included in the model.


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
