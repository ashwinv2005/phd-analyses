log.f = deriv(~k + (a + a1*plot567)*log(size), namevec=c('k', 'a', 'a1'), function.arg=c('size','k', 'a', 'a1', 'plot567'))
fit.nlmer1 = nlmer(rich ~ log.f(size, k, a, a1, plot567) ~ k|site, start=list(nlpars=c(k=2, a=1, a1=1)), data=sgfrag1)

log.f = deriv(~k + a*log(size), namevec=c('k', 'a'), function.arg=c('size','k', 'a'))
fit.nlmer2 = nlmer(rich ~ log.f(size, k, a) ~ k|site, start=list(nlpars=c(k=2, a=1)), data=sfrag[sfrag$size != 124.2,])

log.f = deriv(~k + k1*plot567 + a*log(size), namevec=c('k', 'a', 'k1'), function.arg=c('size','k', 'a','k1','plot567'))
fit.nlmer3 = nlmer(rich ~ log.f(size, k, a, k1, plot567) ~ k|site, start=list(nlpars=c(k=2, a=1, k1=1)), data=sgfrag1)

log.f = deriv(~k + a*log(size) + b*canopy, namevec=c('k', 'a', 'b'), function.arg=c('size','k', 'a', 'b','canopy'))
fit.nlmer4 = nlmer(rich ~ log.f(size, k, a, b, canopy) ~ k|site, start=list(nlpars=c(k=2, a=1, b=1)), data=sgfrag1)

log.f = deriv(~k + (a + a1*plot67 + a2*plot5)*log(size), namevec=c('k', 'a', 'a1', 'a2'), function.arg=c('size','k', 'a', 'a1', 'a2', 'plot67', 'plot5'))
fit.nlmer5 = nlmer(rich ~ log.f(size, k, a, a1, a2, plot67, plot5) ~ k|site, start=list(nlpars=c(k=2, a=1, a1=1, a2=1)), data=sgfrag1)

log.f = deriv(~k + (a + a1*plot57 + a2*plot6)*log(size), namevec=c('k', 'a', 'a1', 'a2'), function.arg=c('size','k', 'a', 'a1', 'a2', 'plot57', 'plot6'))
fit.nlmer6 = nlmer(rich ~ log.f(size, k, a, a1, a2, plot57, plot6) ~ k|site, start=list(nlpars=c(k=2, a=1, a1=1, a2=1)), data=sgfrag1)

log.f = deriv(~k + (a + a1*plot67)*log(size), namevec=c('k', 'a', 'a1'), function.arg=c('size','k', 'a', 'a1', 'plot67'))
fit.nlmer7 = nlmer(rich ~ log.f(size, k, a, a1, plot67) ~ k|site, start=list(nlpars=c(k=2, a=1, a1=1)), data=sgfrag1)

log.f = deriv(~k + (a + a1*plot57)*log(size), namevec=c('k', 'a', 'a1'), function.arg=c('size','k', 'a', 'a1', 'plot57'))
fit.nlmer8 = nlmer(rich ~ log.f(size, k, a, a1, plot57) ~ k|site, start=list(nlpars=c(k=2, a=1, a1=1)), data=sgfrag1)

fit.nls2 = nls(beta1 ~ k + a*log(size), start=list(k=2, a=1), data=sfrag[sfrag$size != 124.2,])


summary(fit.nls2)
anova(fit6,fit7)

plot(sgloc256[sgloc256$richdiv >= 0 & sgloc256$richdiv <= 3,]$shannonsgsgtrans, resid(fit6))

hist(ranef(fit6, type = "response")[[1]][,1], xlab="Intercept", main="", breaks = 10)

plot(sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species == "Climber1",]$canopy, resid(fit6, "response"))
plot(predict(fit6, type = "response"), resid(fit6, "response"))
plot(predict(fit6), resid(fit6))
abline(lm(resid(fit2)~predict(fit2)))

fit5 = lmer(shannonsgsgtrans ~ (1|locationfac), data = sgloc256[sgloc256$richdiv >= 1 & sgloc256$richdiv <= 3,])
fit6 = lmer(shannonsgsgtrans ~ richdiv + plot125 + (1|locationfac), data = sgloc256[sgloc256$richdiv >= 1 & sgloc256$richdiv <= 3,])

summary(fit6)

AIC(fit3)
anova(fit5,fit6)

temp = sgfrag156
a = predict(fit.nlmer7, type = "response", re.form = NA)
b = predict(fit.nlmer7, type = "response")
c = b - a
length(temp$site)
temp$newbeta = temp$beta1 - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

fit9 = glmer(dd ~ 1 + status1 + status1:plot5 + size:plot6 + (1|site), weights = status1, data = temp1[-c(48,418,444)], family="binomial", control=glmerControl(optimizer="bobyqa"))

influence = influence(fit3, obs = T)
cooks=cooks.distance(influence, sort = T)
cooks[cooks>0.3]
plot(cooks)
#[-c(228,559,592,224,423,608,626),]
dfbetas(influence, sort=TRUE, to.sort="status1", abs=FALSE)
temp1 = temp[-c(228, 559, 592, 224, 423, 608, 626), ]
