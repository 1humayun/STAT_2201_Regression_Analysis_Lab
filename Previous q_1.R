df <- data.frame(
   obs = 1:30,
   Force = c(30,40,30,40,30,40,30,40,30,40,30,40,30,40,30,40,25,45,35,35,35,35,35,35,35,35,35,35,35,35),
   Power = c(60,60,90,90,60,60,90,90,60,60,90,90,60,90,90,90,75,75,45,105,75,75,75,75,75,75,75,75,75,75),
   Temperature = c(175,175,175,175,225,225,225,225,175,175,175,175,225,225,225,225,200,200,200,200,150,250,200,200,200,200,200,200,200,200),
   Time = c(15,15,15,15,15,15,15,15,25,25,25,25,25,25,25,25,20,20,20,20,20,20,30,30,30,20,20,20,20,20),
   Strength = c(26.2,26.3,39.8,39.7,38.6,35.5,48.8,37.8,26.6,23.4,38.6,52.1,39.5,32.3,25.4,36.0,35.2,46.9,22.7,58.7,34.5,20.0,35.7,41.8,36.5,40.0,40.3,46.0,27.8,40.3)
)

# Fit initial linear model
model <- lm(Strength ~ Force + Power + Temperature + Time, data = df)

# Plot normality of residuals
qqnorm(resid(model), main = "Normal Q-Q Plot of Residuals")
qqline(resid(model))
hist(resid(model), main = "Histogram of Residuals", xlab = "Residuals")

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(resid(model))
cat("Shapiro-Wilk test p-value:", shapiro_test$p.value, "\n")

########################### (b)
# Calculate diagnostics
df$residuals <- resid(model)
df$standardized_residuals <- rstandard(model)
df$cooks_distance <- cooks.distance(model)
df$leverage <- hatvalues(model)

# Identify outliers (standardized residuals > 2 or < -2)
outliers <- which(abs(df$standardized_residuals) > 2)
cat("Outliers at observations:", outliers, "\n")

# Identify high leverage points (leverage > 2*(p+1)/n where p=number of predictors)


twice_mean = 2*mean(leverage)
high_leverage <- which(df$leverage > twice_mean)
cat("High leverage points at observations:", high_leverage, "\n")

# Identify influential observations (Cook's distance > 4/n)
n <- nrow(df)
influential <- which(df$cooks_distance > 4/n)
cat("Influential observations at observations:", influential, "\n")

#(c) Does the omission of outlier(s) improve the normal probability plot of residuals?

# Remove outliers and recheck normality
if(length(outliers) > 0) {
   df_no_outliers <- df[-outliers, ]
   model_no_outliers <- lm(Strength ~ Force + Power + Temperature + Time, data = df_no_outliers)
   
   par(mfrow = c(1, 2))
   qqnorm(resid(model_no_outliers), main = "Q-Q Plot Without Outliers")
   qqline(resid(model_no_outliers))
   qqnorm(resid(model), main = "Original Q-Q Plot")
   qqline(resid(model))
   
   shapiro_no_outliers <- shapiro.test(resid(model_no_outliers))
   cat("Original Shapiro p-value:", shapiro_test$p.value, "\n")
   cat("Without outliers Shapiro p-value:", shapiro_no_outliers$p.value, "\n")
} else {
   cat("No outliers identified, no improvement possible.\n")
}
# (d) Interpretation if multicollinearity is present and recommendation
# Check for multicollinearity
library(car)
vif_values <- vif(model)
cat("Variance Inflation Factors:\n")
print(vif_values)

# Correlation matrix
cor_matrix <- cor(df[, c("Force", "Power", "Temperature", "Time")])
cat("\nCorrelation matrix:\n")
print(cor_matrix)

#(e) Use an appropriate regression model
# Based on diagnostics, we'll use the original model or a modified version
# If outliers were problematic, we might use robust regression:
library(MASS)
robust_model <- rlm(Strength ~ Force + Power + Temperature + Time, data = df)

# But for this example, we'll proceed with the original linear model
final_model <- model

#(iii) Calculate R² and R²-adjusted and comment
# Calculate R-squared and adjusted R-squared
model_summary <- summary(final_model)
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared

cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n")

# Comment on model adequacy
if(r_squared > 0.7) {
   cat("The model explains a large portion of the variance in Strength.\n")
} else if(r_squared > 0.5) {
   cat("The model explains a moderate amount of variance in Strength.\n")
} else {
   cat("The model explains only a small portion of the variance in Strength.\n")
}

cat("The adjusted R-squared accounts for the number of predictors and is", 
    ifelse(adj_r_squared < r_squared, "lower", "equal"), "than R-squared.\n")

#(g) Check significance of parameters and test hypothesis
# Summary of model to see significance of parameters
cat("Model summary:\n")
print(summary(final_model))

# Test H0: β1 = 1.2
beta1 <- coef(final_model)["Force"]
beta1_se <- summary(final_model)$coefficients["Force", "Std. Error"]
t_value <- (beta1 - 1.2) / beta1_se
p_value <- 2 * pt(abs(t_value), df = df.residual(final_model), lower.tail = FALSE)

cat("\nTest of H0: β1 = 1.2\n")
cat("t-value:", t_value, "\n")
cat("p-value:", p_value, "\n")

# 90% confidence interval for β1
ci_beta1 <- confint(final_model, "Force", level = 0.90)
cat("\n90% confidence interval for β1:\n")
print(ci_beta1)

