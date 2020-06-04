# Chapter 4: Correlation and Regression
# Summary:

# good 2 haves!
rm(list = ls())  # Clean up global environment 
dev.off()        # Remove plots

# import data
# Let's read the dataset into R
cost <- read.table( 'Cost_of_Living_2013.txt', sep='\t', header=TRUE) 

# We should analyze a few observations 
head(cost)    # read first 6 rows
tail(cost)    # read last 6 rows

# Then get an idea of the data structure
str(cost)     # structure of an r object
dim(cost)     # cols and rows similar to shape in python
typeof(cost)  # type of dataset 
names(cost)   # column names
nrow(cost)    # total number of rows
ncol(cost)    # total number of columns

# We are asked to produce a scatterplot between Cost.of.Living.Index and each of the other variables. 
# What I'd like to do is create a scatter plot of the entire dataframe
# This is ideal if you're working with less than 6 variables 
pairs(cost, main="Are there any weak or strong relationships?", pch=16, cex=.5)

# We can quickly look at the relation between each variable in our dataframe 
# And we see that $City doesn't have any relation with the other variables

# Now let's plot $Cost.of.Living against each variable

plot(Cost.of.Living.Index ~ Rent.Index, data = cost,
     pch = 16,         # Solid circle
     cex = 1.5,         # Make 50% size
     main = "Cost of Living Index vs. Rent Index",
     xlab = "Rent Index",
     ylab = "Cost of Living Index")

plot(Cost.of.Living.Index ~ Restaurant.Price.Index, data=cost,
     pch = 16,
     cex = .5,
     main = "Cost of Living Index vs. Restaurant Price Index",
     xlab = "Restaurant Price Index",
     ylab = "Cost of Living Index")

plot(Cost.of.Living.Index ~ Groceries.Index, data=cost,
     pch = 16,         
     cex = .5,       
     main = "Cost of Living Index vs. Groceries Index",
     xlab = "Groceries Index",
     ylab = "Cost of Living Index")

plot(Cost.of.Living.Index ~ Local.Purchasing.Power.Index, data=cost,
     pch = 16,         
     cex = .5,         
     main = "Cost of Living Index vs. Local Purchasing Power Index",
     xlab = "Local Purchasing Power Index",
     ylab = "Cost of Living Index")

# Our scatterplots follow a positive linear slope 
# But hard to determine which has the strongest correlation
# Let's compute the correlation for all plots 
cor(cost[,2], cost[,-c(1,2) ] ) # Cost of Living Index vs. other variables. Excluding 1st and 2nd column.

# We see that $Groceries.Index has a correlation of 0.954.
# We must verify all conditions for each correlation
# Recall this:
#  quantitative variable condition
#  linearity condition
#  outlier condition

# Let's  check!
# We know that our variables are quantitative.
str(cost[2:6])

# All scatterplots follow a linear form.
par(mfrow=c(2,2))

plot(Cost.of.Living.Index ~ Rent.Index, data = cost)
plot(Cost.of.Living.Index ~ Restaurant.Price.Index, data=cost)
plot(Cost.of.Living.Index ~ Groceries.Index, data=cost)
plot(Cost.of.Living.Index ~ Local.Purchasing.Power.Index, data=cost)

par(mfrow=c(1,1)) # reset plot 

# There's only 2 outliers in the $Rent.Index plot. The rest of the plots passed the outlier condition. 

# Let's reorder the columns to avoid confusion 
# as we plan to set the index to the names of the cities

library(ggplot2) # import ggplot package
head(cost)

df2 <- cost[,c(2,3,4,5,6,1)] # re-order columns
head(df2)
row.names(df2) <- df2$City # set row index to column$City
head(df2)

# Now let's plot the outliers and label them with the name of the city 
outliers <- ggplot(df2, aes(Rent.Index, Cost.of.Living.Index))
outliers + geom_point()
outliers + geom_point() + geom_text(data=subset(df2, Rent.Index > 120),
                             hjust = 1.1, 
                             aes(Rent.Index, label=City))

# We see that Hong Kong and Luanda are the outliers.
# Hong Kong as the usual suspect, but Luanda? More expensive than New York?
# How is that possible? 

# Now let's create linear regression model between $Cost.of.Living.Index and each variables.
# Interpret the slope in each model.

mod1 <- lm(Cost.of.Living.Index ~ Rent.Index, data=cost)
mod1$coeff  # $Rent.Index slope = 1.025
# The slope of Rent.Index is 1.025, which means the Cost.of.Living.Index on average increases
# by 1.025% as the Rent.Index increases by 1%.

mod2 <- lm(Cost.of.Living.IndeX ~ Groceries.Index, data=cost)
mod2$coeff  # $Groceries.Index slope = 0.953
# The slope of Groceries.Index is 0.953, which means the Cost.of.Living.Index on average increases
# by 0.953% as the Groceries.Index increases by 1%.

mod3 <- lm(Cost.of.Living.Index ~ Restaurant.Price.Index, data=cost)
mod3$coeff  # $Restaurant.Price.Index slope = 0.803
# The slope of Restaurant.Price.Index is 0.803, which means the Cost.of.Living.Index on average increases
# by 0.803% as the Restaurant.Price.Index increases by 1%.

mod4 <- lm(Cost.of.Living.Index~Local.Purchasing.Power.Index, data=cost)
mod4$coeff  # $Local.Purchasing.Power.Index slope = 0.376
# The slope of Local.Purchasing.Power.Index is 0.376, which mean the Cost.of.Living.Index on average increases
# by 0.376% as the Local.Purchasing.Power.Index increases by 1%.

# Based on the correlation that we already computed, which models best predicts the overall cost 
# in these cities and vice versa.
# Let's call back the correlation we created!
cor(cost[,2], cost[,-c(1,2) ] )

# Now find the highest and lowest R-squared!
summary(mod1)
summary(mod1)$r.squared

summary(mod2)
summary(mod2)$r.squared # best predictor, high correlation = 0.954 & R-squared = 0.910 

summary(mod3)
summary(mod3)$r.squared 

summary(mod4)
summary(mod4)$r.squared # worst predictor, low correlation = 0.526 & R-squared = 0.277

# We can conclude the best predictor of overall cost is $Groceries.Index 
# because it has the highest correlation and R-squared.

# Suppose we want to find the cost of living predicted by Groceries.Index and it's residual 
# for Beijing, China. 

beij.index <- which(cost$City == 'Beijing, China') # position of Beijing in the dataset
beij.index
is.vector(beij.index)

mod2$fitted.values[beij.index] # predicted values for Beijing, China
mod2$residuals[beij.index] # y~hat is an overestimate

# clean up console!
cat("\014")      # ctrl+L 


######### testing #########

ny.index <- which(cost$City == 'New York, NY, United States')
ny.index
is.vector(ny.index)

mod2$fitted.values[ny.index]
mod2$residuals[ny.index]

# beijing seems to be less expensive by 11.14%
ny.beij <-  cost$Groceries.Index[ny.index] - mod2$fitted.values[beij.index]
ny.beij
testing

# clean up console!
cat("\014")      # ctrl+L 

