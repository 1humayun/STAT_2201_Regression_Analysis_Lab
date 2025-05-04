########################## Problem 5 | (Property data) ###############
# Given data
y = c(25.9,29.5,27.9,25.9,29.9,29.9,30.9,28.9,45.8,36.9,38.9,37.9,44.5,37.9,37.5,43.9)
x2 = c(3.472,3.531,2.275,4.050,4.455,4.455,5.850,9.520,7.326,8.000,9.150,6.727,9.890,5.000,5.520,7.800)
x3 = c(42,60,40,54,42,56,51,32,31,3,48,44,50,22,40,23)

# Create a dataframe
df = data.frame(y, x2, x3)
head(df)

# (a) Fit the multiple linear regression model
model = lm(y ~ x2 + x3, data = df)
summary(model)

# (b) Compute R-squared and Adjusted R-squared
r_squared = summary(model)$r.squared
adj_r_squared = summary(model)$adj.r.squared
cat("R-squared:", r_squared, "\nAdjusted R-squared:", adj_r_squared, "\n")

# (c) ANOVA table
anova(model)


# (d) 90% Confidence and Prediction Intervals for x1=10, x2=50
new_data = data.frame(x2 = 10, x3 = 50)
conf_int = predict(model, new_data, interval = "confidence", level = 0.90)
pred_int = predict(model, new_data, interval = "prediction", level = 0.90)

cat("Fit: ", conf_int[1])
cat("90% Confidence Interval:", conf_int[c(2,3)], "\n")
cat("90% Prediction Interval:", pred_int[c(2,3)], "\n")

# (e) Standardized Regression Coefficients
std_x2 = sd(df$x2)
std_x3 = sd(df$x3)
std_y = sd(df$y)

beta_x2 = coef(model)["x2"] * (std_x2 / std_y)
beta_x3 = coef(model)["x3"] * (std_x3 / std_y)

cat("Standardized Coefficient for x2:", beta_x2, "\n")
cat("Standardized Coefficient for x3:", beta_x3, "\n")

# (f) Interaction Effect
model_interaction = lm(y ~ x2 + x3 + x2:x3, data = df)
summary(model_interaction)
