# Regression Models - week 4

# 1. Consider the space shuttle data ?shuttle in the MASS library.
# Consider modeling the use of the autolander as the outcome (variable name use).
# Fit a logistic regression model with autolander (variable auto) use (labeled as "auto" 1)
# versus not (0) as predicted by wind sign (variable wind).
# Give the estimated odds ratio for autolander use comparing head winds,
# labeled as "head" in the variable headwind (numerator) to tail winds (denominator).

# Use shuttle dataset from MASS library
library(MASS)

# Explore dataset manually
with(data=shuttle, table(use, wind))
p0 <- 72/(72+56)
b0 <- log(p0 / (1-p0))
p1 <- 73/(73+55)
b1 <- log(p1 / (1-p1)) - b0

# Relevel factor levels (noauto=0)
shuttle$use <- relevel(shuttle$use, ref = "noauto")
shuttle$wind <- relevel(shuttle$wind, ref = "head")
shuttle$magn <- relevel(shuttle$magn, ref = "Out")

# GLM coefficients
fit1 <- glm(factor(use) ~ factor(wind), family="binomial", data=shuttle)
fit1_b0 <- summary(fit1)$coefficients[[1,1]]
fit1_b1 <- summary(fit1)$coefficients[[2,1]]
fit1

# Odds ratio (head / tail)
exp(-fit1_b1)

# 2. Give the estimated odds ratio for autolander use comparing head winds (numerator)
# to tail winds (denominator) adjusting for wind strength from the variable magn.
fit2 <- glm(factor(use) ~ factor(wind) + factor(magn), family="binomial", data=shuttle)
fit2_b0 <- summary(fit2)$coefficients[[1,1]]
fit2_b1 <- summary(fit2)$coefficients[[2,1]]
exp(-fit2_b1)

# 3. If you fit a logistic regression model to a binary variable,
# for example use of the autolander, then fit a logistic regression model for one minus
# the outcome (not using the autolander) what happens to the coefficients?

glm(I(use!="auto") ~ factor(wind), family="binomial", data=shuttle)

# 4. Consider the insect spray data InsectSprays. Fit a Poisson model using spray as a factor level.
# Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).

# Explore dataset InsectSprays
plot(count ~ spray, data=InsectSprays)

# GLM
fit_spray <- glm(count ~ factor(spray), data=InsectSprays, family="poisson")

# Relative rate lambda_A/lambda_B = exp(eta_A) / exp(eta_B) = exp(eta_A - eta_B) = exp(-b_B)
b_B <- fit_spray$coefficients[2]
exp(-b_B)


# 5. Consider a Poisson glm with an offset, t. So, for example, a model of the form
# glm(count ~ x + offset(t), family = poisson) where x is a factor variable
# comparing a treatment (1) to a control (0) and t is the natural log of a monitoring time.
# What is impact of the coefficient for x if we fit the model
# glm(count ~ x + offset(t2), family = poisson) where t2 <- log(10) + t?
# In other words, what happens to the coefficients if we change the units
# of the offset variable. (Note, adding log(10) on the log scale is multiplying by 10
# on the original scale.)

# 6. Consider the data
# x <- -5:5
# y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
# Using a knot point at 0, fit a linear model that looks like a hockey stick
# with two lines meeting at x=0. Include an intercept term, x and the knot point term.
# What is the estimated slope of the line after 0?

# Load data
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

# Explore data
plot(x,y)

# Fit hockeystick model
knotterm <- sapply(x, function(i) {max(0, i)})
fit <- lm(y ~ 1 + x + knotterm)
fit$coefficients[2] + fit$coefficients[3]
