#(a) Logistic Regression to Assess Impact of Temperature on Failure
# Create the dataset
temperature <- c(53, 56, 57, 63, 66, 67, 67, 67, 68, 69, 70, 70, 70, 70, 72, 73, 75, 75, 76, 76, 78, 79, 80, 81)
failure <- c("Y", "Y", "Y", "N", "N", "N", "N", "N", "N", "N", "N", "Y", "Y", "Y", "N", "N", "N", "Y", "N", "N", "N", "N", "N", "N")

# Convert failure to binary (1 for Y, 0 for N)
failure_binary <- ifelse(failure == "Y", 1, 0)

# Fit logistic regression model
oring_model <- glm(failure_binary ~ temperature, family = binomial(link = "logit"))

# Display model summary
summary(oring_model)

# Check significance of temperature
cat("\nSignificance of temperature effect:\n")
coef_summary <- summary(oring_model)$coefficients
p_value <- coef_summary["temperature", "Pr(>|z|)"]
cat("p-value:", p_value, "\n")

if(p_value < 0.05) {
   cat("Temperature has a statistically significant impact on O-ring failure (p < 0.05).\n")
} else {
   cat("Temperature does not have a statistically significant impact on O-ring failure (p ≥ 0.05).\n")
}

# Plot the logistic regression curve
plot(temperature, failure_binary, xlab = "Temperature (°F)", ylab = "Probability of Failure", 
     main = "O-ring Failure vs. Temperature")
curve(predict(oring_model, data.frame(temperature = x), type = "response"), 
      add = TRUE, col = "red", lwd = 2)


#(b) Probability of Failure at 40°F
# Predict probability at 40°F
new_data <- data.frame(temperature = 40)
pred_prob <- predict(oring_model, newdata = new_data, type = "response")

cat("\nPredicted probability of failure at 40°F:", pred_prob, "\n")
cat("This means there's a", round(pred_prob * 100, 1), "% chance of O-ring failure at 40°F.\n")

#Extra...
# Additional visualization for better understanding
temp_range <- seq(30, 85, by = 1)
probs <- predict(oring_model, data.frame(temperature = temp_range), type = "response")

plot(temp_range, probs, type = "l", col = "blue", lwd = 2,
     xlab = "Temperature (°F)", ylab = "Probability of Failure",
     main = "O-ring Failure Probability vs. Temperature")
points(temperature, failure_binary)
abline(v = 40, col = "red", lty = 2)
text(40, 0.9, "40°F", pos = 4, col = "red")


