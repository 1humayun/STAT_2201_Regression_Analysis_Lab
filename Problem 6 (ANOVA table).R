########################## Problem 6 | (ANOVA Table Analysis) ###############
# Given data
SSR = 260.1784  # Regression Sum of Squares
SST = 262.6886  # Total Sum of Squares
df_total = 39   # Total Degrees of Freedom
df_error = 34   # Error Degrees of Freedom

# Compute SSE (Error Sum of Squares)
SSE = SST - SSR  # Error Sum of Squares

# Compute df for regression
df_regression = df_total - df_error  # Degrees of Freedom for Regression

# (i) Estimated standard error of regression, s
MSE = SSE / df_error  # Mean Square Error
s = sqrt(MSE)  # Standard error of regression

# (ii) Coefficient of multiple determination (R^2)
R_squared = SSR / SST

# (iii) Adjusted R-squared
adj_R_squared = 1 - ((SSE / df_error) / (SST / df_total))

# (iv) F-statistic for goodness of fit
MSR = SSR / df_regression  # Mean Square for Regression
F_statistic = MSR / MSE  # F-test value

# Print results
cat("Estimated Standard Error (s):", s, "\n")
cat("Coefficient of Multiple Determination (R^2):", R_squared, "\n")
cat("Adjusted R^2:", adj_R_squared, "\n")
cat("F-statistic:", F_statistic, "\n")

# (vi) Comment on model fitting
if (R_squared > 0.8) {
   cat("The model fits the data very well.\n")
} else if (R_squared > 0.5) {
   cat("The model has a moderate fit.\n")
} else {
   cat("The model does not fit the data well.\n")
}
