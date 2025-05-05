#############################		Property Valuation data		#######################

df = data.frame(	y = c(25.9,29.5,27.9,25.9,29.9,29.9,30.9,28.9,45.8,36.9,38.9,37.9,44.5,37.9,37.5,43.9),
			x2 = c(3.472,3.531,2.275,4.050,4.455,4.455,5.850,9.520,7.326,8.000,9.150,6.727,9.890,5.000,5.520,7.800),
			x3 = c(42,62,40,54,42,56,51,32,31,3,48,44,50,22,40,23)
)

head(df)

model = lm(y ~ x2 + x3, data=df)  # full model
summary(model)

r_sq = summary(model)["r.squared"];r_sq
adj_r_sq = summary(model)["adj.r.squared"];adj_r_sq

null_model <- lm(y ~ 1, data = df) # Only intercept, no predictors model
anova(null_model, model)

# Since p=0.015 < 0.05, The full model (with x2, x3, and their interaction) is statistically significant compared to the intercept-only model.

new.data = data.frame(x2= 10, x3 = 50)
conf_int = predict(model, new.data, interval= "confidence", level = .90);conf_int
pred_int = predict(model, new.data, interval = "prediction", level = .90);pred_int

sd.y = sd(df$y)
sd.x2 = sd(df$x2)
sd.x3 = sd(df$x3)

beta_x2 = coef(model)["x2"]* (sd.x2/sd.y)
beta_x3 = coef(model)["x3"]*(sd.x3/sd.y)

model_int = lm(y~x2+x3+x2:x3, data = df)
summary(model_int)



#############################		ANOVA Table		#######################

RSS = 260.1784
TSS = 262.6886
e.df = 34
total.df = 39
reg.df = total.df - e.df
ESS = TSS - RSS

RMS = RSS/reg.df
EMS = ESS/e.df
TMS = TSS/total.df

r.sq = RSS/TSS; r.sq
cor.r.sq = 1- (EMS/TMS); cor.r.sq
adj.r.sq = 

f.stat = RMS/EMS
f.stat


#############################		Chatterjee and Hadi		#######################
df = data.frame(y = c(1, 14, 25, 34, 41, 46, 49, 50, 49, 46, 41, 34, 25, 14, 1),
			x = c(-7:7))
head(df)
df

model = lm(y~x, data = df)
summary(model)

plot(df$x,df$y, main = "Y vs X Plot", xlab = "X", ylab = "Y", pch = 16)
lines(df$x,df$y, col = "red", lwd = 2)

plot(df$x, residuals(model), main = "Residuals vs X Plot", 
	xlab = "X", ylab = "Residuals", col= "blue", pch = 16)
lines(df$x, residuals(model), col= "green", lwd=2)


#############################		Water flow data		#######################
x = c(27.1, 20.9, 33.4, 77.6, 37.0, 21.6, 17.6, 35.1, 32.6, 26.0, 27.6, 38.7, 27.8)
y = c(19.7, 18.0, 26.1, 15.7, 26.1, 19.9, 15.7, 27.6, 24.9, 23.4, 23.1, 31.3, 23.8)

df = data.frame(y,x)
head(df)

model = lm(y~x, data= df)
summary(model)

res = residuals(model)
stu.res = rstudent(model)
del.stu.res = rstudent(model)

qqnorm(res, main = "Normal Probability plot of Residuals")
qqline(res, col = "blue", lwd = 2)

qqnorm(stu.res, main = "Normal prob plot of Stu res")
qqline(stu.res, col="red", lwd = 2)

qqnorm(del.stu.res, main = "Normal prob plot of del Stu res")
qqline(del.stu.res, col="red", lwd = 2)

outlier = which(abs(stu.res)>2)
outlier

if(length(outlier)) cat("Outliers at: ", toString(outlier), "\n") else cat("No outliers")

leverage = hatvalues(model)
twice_mean = 2*mean(leverage)
thrice_mean = 3*mean(leverage)

high.lev = which(leverage > twice_mean)
high.lev
if(length(high.lev)) cat("High Leverage value at: ", toString(high.lev), "\n") else cat("No High Leverage Value")

cook.dis = cooks.distance(model)
DFFITS = dffits(model)

cook.threshold = 4/nrow(df)

inf.cooks = which(abs(cook.dis)>cook.threshold)
inf.cooks
inf.dffits = which(abs(DFFITS) >2*sqrt(mean(leverage)))
inf.dffits

if(length(inf.cooks)) cat("Influential observation based on Cooks Distance at: ", inf.cooks, "\n") else cat("No Influential Obs")
if(length(inf.dffits)) cat("Influential observation based on DFFITS at: ", inf.dffits, "\n") else cat("No influential")

if(length(outlier)>0){
df.clean = df[-outlier, ]
model.clean = lm(y~x, data = df.clean)
new.res = residuals(model.clean)
qqnorm(new.res, main="QQ plot after removing outliers.")
qqline(new.res, col = "blue")
}


#####################################
x = c(27.1, 20.9, 33.4, 77.6, 37.0, 21.6, 17.6, 35.1, 32.6, 26.0, 27.6, 38.7, 27.8)
y = c(19.7, 18.0, 26.1, 15.7, 26.1, 19.9, 15.7, 27.6, 24.9, 23.4, 23.1, 31.3, 23.8)

df = data.frame(y,x)

model = lm(y~x, data= df)

res = residuals(model)
stu.res = rstudent(model)
del.stu.res = rstudent(model)

qqnorm(res, main = "Normal Probability plot of Residuals")
qqline(res, col = "blue", lwd = 2)

qqnorm(stu.res, main = "Normal prob plot of Stu res")
qqline(stu.res, col="red", lwd = 2)

qqnorm(del.stu.res, main = "Normal prob plot of del Stu res")
qqline(del.stu.res, col="red", lwd = 2)

outlier = which(abs(stu.res)>2)
outlier

leverage = hatvalues(model)
twice_mean = 2*mean(leverage)
thrice_mean = 3*mean(leverage)

high.lev = which(leverage > twice_mean)
high.lev

cook.dis = cooks.distance(model)
DFFITS = dffits(model)

cook.threshold = 4/nrow(df)

inf.cooks = which(abs(cook.dis)>cook.threshold)
inf.cooks
inf.dffits = which(abs(DFFITS) >2*sqrt(mean(leverage)))
inf.dffits

if(length(outlier)>0){
df.clean = df[-outlier, ]
model.clean = lm(y~x, data = df.clean)
new.res = residuals(model.clean)
qqnorm(new.res, main="QQ plot after removing outliers.")
qqline(new.res, col = "blue")
}


#######################		Logistic Regression	################################
df = data.frame(
   Income = c(4000, 3000, 5000, 2500, 6000, 4500, 3200, 5800, 2200, 4900),
   CreditScore = c(700, 650, 720, 600, 750, 710, 640, 730, 580, 705),
   LoanAmount = c(20000, 15000, 25000, 10000, 27000, 22000, 16000, 26000, 9000, 24000),
   Married = c("Yes", "No", "Yes", "No", "Yes", "Yes", "No", "Yes", "No", "Yes"),
   Approved = c(1, 0, 1, 0, 1, 1, 0, 1, 0, 1)
)

df$Married.dummy = ifelse(df$Married == "Yes", 1, 0)
model = glm(Approved~Income + CreditScore + LoanAmount + Married.dummy, family = binomial(link = "logit"), data=df)
summary(model)

new.applicant = data.frame(
Income = 4500,
CreditScore = 700,
LoanAmount = 20000,
Married.dummy = 1
)

prob = predict(model, newdata = new.applicant, type = "response")
prob
cat("Probability of Loan approval in Percentage: ", round(prob*100, 1), "%\n")