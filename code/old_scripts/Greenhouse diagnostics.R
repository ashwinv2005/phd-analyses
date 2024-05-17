plot(predict(fit), resid(fit,"response"), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
lines(lowess(predict(fit),resid(fit)),col="black",lwd=2)
plot(temp$weightm, resid(fit,"response")) # residuals vs. predictor 1
plot(temp$sizem, resid(fit, "response")) # residuals vs. predictor 2

qqnorm(resid(fit))

plot(temp$initial ~ temp$treatment)
plot(temp$final1 ~ temp$weight)
plot(predict(fit)~temp$weight)

plot(binomial()$linkinv(predict(fit))~temp$size)

temp$final2 = as.numeric(temp$final1)
temp1 = temp[temp$treatment == "C",]
ini = summarySE(temp1, groupvars = "species", measurevar = "final2")
wt = summarySE(temp1, groupvars = "species", measurevar = "meanweight")

wt$median = c(NA, median(temp1[temp1$species == "OD",]$weight), median(temp1[temp1$species == "SC",]$weight),
              median(temp1[temp1$species == "SG",]$weight), median(temp1[temp1$species == "SR",]$weight),
              median(temp1[temp$species == "Symp",]$weight),NA)

plot(ini$final2 ~ wt$meanweight)
abline(lm(ini$final2 ~ wt$meanweight))
summary(lm(ini$final2 ~ wt$meanweight))

hist(ranef(fit)[[1]][,1], xlab="Intercept", main="", breaks = 20) # random effects
