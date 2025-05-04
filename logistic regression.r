df <- data.frame(
   Income = c(4000, 3000, 5000, 2500, 6000, 4500, 3200, 5800, 2200, 4900),
   CreditScore = c(700, 650, 720, 600, 750, 710, 640, 730, 580, 705),
   LoanAmount = c(20000, 15000, 25000, 10000, 27000, 22000, 16000, 26000, 9000, 24000),
   Married = c("Yes", "No", "Yes", "No", "Yes", "Yes", "No", "Yes", "No", "Yes"),
   Approved = c(1, 0, 1, 0, 1, 1, 0, 1, 0, 1)
)

plot(df$Income~df$LoanAmount)
# View the dataset
print(df)

# Create dummy variable for Married (Yes = 1, No = 0)
df$Married_dummy <- ifelse(df$Married == "Yes", 1, 0)  # Fixed assignment operator

# Fit logistic regression model
model <- glm(Approved ~ Income + CreditScore + LoanAmount + Married_dummy, 
             family = binomial(link = "logit"), 
             data = df)

# Summary of the model
summary(model)


# New applicant data
new_applicant <- data.frame(
   Income = 4500,
   CreditScore = 700,
   LoanAmount = 20000,
   Married_dummy = 1  # 1 = "Yes", 0 = "No"
)

# Predict probability of approval
prob <- predict(model, newdata = new_applicant, type = "response")

# Print the probability (formatted as percentage)
cat("Probability of loan approval:", round(prob * 100, 1), "%\n")

