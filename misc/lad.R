# Simulate 30 obs from a simple linear regression model
set.seed(922)  # for reproducibility
x <- runif(30, min = 0, max = 10)
y <- 1 + 2*x + rnorm(30, sd = 2)

# Inspect LS coefficients
coef(lm(y ~ x))

# Plot data with fitted regression line
par(mfrow = c(1, 2))
plot(x, y)
abline(coef(lm(y ~ x)))

# Make one of the points an extreme outlier and re-plot the results
y[which.max(y)] <- -50
plot(x, y)
abline(coef(lm(y ~ x)))

coef(lm(y ~ x))

# Least absolute deviation (LAD) regression criteria
lad <- function(betas) {  
 sum(abs(y - betas[1] - betas[2]*x))
}

# Find intercept and slope that minimize LAD loss
res <- optim(par = coef(lm(y ~ x)), fn = lad)

# Inspect results
abline(res$par, lty = 2, col = "red2", lwd = 2)  # add to previous plot
res$par
