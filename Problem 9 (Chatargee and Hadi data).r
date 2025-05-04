############################# Problem 9 | (Chatargee and Hadi data) #####################


# Define the data
X = c(-7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7)
Y = c(1, 14, 25, 34, 41, 46, 49, 50, 49, 46, 41, 34, 25, 14, 1)

# Create a data frame
df = data.frame(X, Y)

# Fit a linear regression model
linear_model = lm(Y ~ X, data = df)

# Print summary of the model
summary(linear_model)
anova(linear_model)

# Create a scatter plot
plot(X, Y, 
     main = "Scatter Plot of Nonlinear Data", 
     xlab = "X", 
     ylab = "Y", 
     col = "blue", 
     pch = 16)  # Adjusts point size

# Add a line connecting points to visualize the trend
lines(X, Y, col = "red", lwd = 2)


# Create scatter plot of original data
plot(X, residuals(linear_model), 
     main = "Residual vs X plot", 
     xlab = "X", 
     ylab = "Residuals", 
     col = "blue", 
     pch = 16)

# Add quadratic regression curve
lines(X, residuals(linear_model), col = "red", lwd = 2)
