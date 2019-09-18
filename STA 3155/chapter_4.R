cost <- read.table('Cost_of_Living_2013.txt', sep='\t', header=TRUE)
head(cost)
pairs(cost)

# 1. Produce a scatterplot between the Cost of Living Index and EACH of the other index variables. As a
# result, there should be 4 scatterplots in total. Examine the relationship shown in each scatterplot in
# terms of its form, strength and direction.
plot(Cost.of.Living.Index~Rent.Index, data=cost)
plot(Cost.of.Living.Index~Restaurant.Price.Index, data=cost)
plot(Cost.of.Living.Index~Groceries.Index, data=cost)
plot(Cost.of.Living.Index~Local.Purchasing.Power.Index, data=cost)

# 2. Compute the correlation coefficients for all the scatterplots obtained above.
new_cost <- cost[,c(2,3,4,5,6)]
head(new_cost)
names(new_cost)
pairs(new_cost)

# 3. Verify the conditions for EACH correlation coefficient computed above.
cor(new_cost)

# 4. Fit a linear regression model between the Cost of Living Index and each of the other index variables.
# As a result, there should be 4 regression models in total. Interpret the resulting estimated slope in each
# model.
rent.index <-lm(Cost.of.Living.Index~Rent.Index,data=cost)
groc.index <-lm(Cost.of.Living.Index~Groceries.Index,data=cost)
rest.price.index <-lm(Cost.of.Living.Index~Restaurant.Price.Index,data=cost)
local.pow.index <-lm(Cost.of.Living.Index~Local.Purchasing.Power.Index,data=cost)

# 5. Based on the correlation coefficients and the regression models obtained above, which item would be
# the best predictor of overall cost in these cities? Which would be the worst? Explain.
Based on the cor coeff and regression models above, the best predictors for overall cost of living
in these cities would be "Restaurant.Price.Index" and "Groceries.Index" because both of their p-values
are less than 0.05 which is statistically significant and it will give us a reliable guess of the true living
cost in these cities. To add, their R-squared are respectively high, .80 for "Restaurant.Price.Index" and .95 
for "Groceries.Index". This tells us that both of variables can explain 80% and 95% of the variation in "Cost.Of.Living.Index"

# 6. Find the cost of living as predicted by Groceries Index and its residual for Beijing, China. (Hint: Find
# row index of Beijing in the dataset, and then use that index to extract the corresponding fitted value
# and residual from the regression result.)
