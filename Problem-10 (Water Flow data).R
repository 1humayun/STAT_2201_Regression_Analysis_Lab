
# Define the dataset
x1 = c(27.1, 20.9, 33.4, 77.6, 37.0, 21.6, 17.6, 35.1, 32.6, 26.0, 27.6, 38.7, 27.8)
y1 = c(19.7, 18.0, 26.1, 15.7, 26.1, 19.9, 15.7, 27.6, 24.9, 23.4, 23.1, 31.3, 23.8)

df = data.frame(x1, y1)

# Fit a linear regression model
model = lm(y1 ~ x1, data = df)

# Compute residuals and studentized residuals
residuals = residuals(model)
stud_residuals = rstudent(model)  # Studentized residuals
deleted_stud_residuals = rstudent(model)  # Same as stud_residuals in base R

# (a) Normal Probability Plot
qqnorm(residuals, main = "Normal Probability Plot of Residuals")
qqline(residuals, col = "red")  # Adds a reference line

qqnorm(stud_residuals, main = "Normal Probability Plot of Studentized Residuals")
qqline(stud_residuals, col = "blue")

qqnorm(deleted_stud_residuals, main = "Normal Probability Plot of Deleted Studentized Residuals")
qqline(deleted_stud_residuals, col = "green")

# (b) Identify Outliers using Studentized Residuals
outliers = which(abs(stud_residuals) > 2)  # Mark observations with residuals > 2

if(length(outliers) > 0) {
   print(paste("Outliers detected at indices:", paste(outliers, collapse = ", ")))
} else {
   print("No significant outliers detected.")
}

# (c) Identify High Leverage Points
leverage = hatvalues(model)
threshold_2mean = 2 * mean(leverage)
threshold_3mean = 3 * mean(leverage)

high_leverage_points = which(leverage > threshold_2mean)

if(length(high_leverage_points) > 0) {
   print(paste("High leverage points detected at indices:", paste(high_leverage_points, collapse = ", ")))
} else {
   print("No high leverage points detected.")
}

# (d) Identify Influential Observations using Cook's Distance & DFFITS
cooks_dist = cooks.distance(model)
dffits_values = dffits(model)

# Threshold for Cook's Distance: 4/n (where n = number of observations)
cook_threshold = 4 / nrow(df)

influential_cooks = which(cooks_dist > cook_threshold)
influential_dffits = which(abs(dffits_values) > 2 * sqrt(mean(leverage)))

if(length(influential_cooks) > 0) {
   print(paste("Influential points based on Cook's Distance at indices:", paste(influential_cooks, collapse = ", ")))
} else {
   print("No influential points based on Cook's Distance.")
}

if(length(influential_dffits) > 0) {
   print(paste("Influential points based on DFFITS at indices:", paste(influential_dffits, collapse = ", ")))
} else {
   print("No influential points based on DFFITS.")
}

# (e) Re-fit Model Without Outliers and Check Normality Again
if(length(outliers) > 0) {
   df_clean = df[-outliers, ]  # Remove outliers
   model_clean = lm(y1 ~ x1, data = df_clean)
   
   new_residuals = residuals(model_clean)
   
   qqnorm(new_residuals, main = "QQ Plot After Removing Outliers")
   qqline(new_residuals, col = "red")
   
   print("QQ plot after removing outliers plotted. Compare with original to see improvement.")
}


