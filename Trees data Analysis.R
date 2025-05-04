data = trees
X1 = c(data$Girth)
X1
X2 = data$Height
X2
Y = data$Volume
fit = lm(Y~X1+X2)
fit

deviance(lm(Y~1))
deviance(fit)

fit.residuals = resid(fit)
fit.residuals


ks.test(fit.residuals, "pnorm", mean = mean(fit.residuals),
        sd = sd(fit.residuals))

shapiro.test(fit.residuals)

hist(fit.residuals)

qqnorm(fit.residuals)
qqline(fit.residuals)

